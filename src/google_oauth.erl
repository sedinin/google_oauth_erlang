-module(google_oauth).

-export([get_access_token/2]).

-define(GRANT_TYPE, <<"&grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer">>).
-define(REQ_OPTS, [{full_result, false}, {body_format, binary}]).
-define(JSX_OPTS, [return_maps, {labels, atom}]).

get_access_token(ServiceAccountFile, Scope) when is_list(Scope) ->
    get_access_token(ServiceAccountFile, erlang:list_to_binary(Scope));
get_access_token(ServiceAccountFile, Scope) ->
    {ok, Bin} = file:read_file(ServiceAccountFile),
    ServiceJson = jsx:decode(Bin, ?JSX_OPTS),
    JWTToken = make_jwt(ServiceJson, Scope),
    validate_token(ServiceJson, JWTToken).

validate_token(ServiceJson, JWTToken) ->
    Body = iolist_to_binary([<<"assertion=">>, JWTToken, ?GRANT_TYPE]),
    Uri = get_uri(ServiceJson),
    Req = {Uri, [], "application/x-www-form-urlencoded", Body},
    case httpc:request(post, Req, [], ?REQ_OPTS) of
        {ok, {200, Result}} -> {ok, jsx:decode(Result, ?JSX_OPTS)};
        {ok, Reason} -> {error, Reason};
        {error, _} = Error -> Error
    end.

get_uri(#{token_uri  := TokenUri}) when is_binary(TokenUri) ->
    erlang:binary_to_list(TokenUri);
get_uri(#{token_uri  := TokenUri}) ->
    TokenUri.

make_jwt(Json, Scope) ->
    #{
        token_uri           := TokenUri,
        private_key         := EncodedPrivateKey,
        private_key_id      := PrivateKeyId,
        client_email        := ClientEmail
     } = Json,

     Header = #{
        typ     => <<"JWT">>,
        alg     => <<"RS256">>,
        kid     => PrivateKeyId
     },

     EncodedJWTHeader = base64url:encode(jsx:encode(Header)),

     Now = erlang:system_time(seconds),

     Claims = #{
        aud     => TokenUri,
        scope   => Scope,
        iat     => Now,
        exp     => Now + 3600,
        iss     => ClientEmail
     },

     EncodedJWTClaimSet = base64url:encode(jsx:encode(Claims)),

     [PemEntry|_] = public_key:pem_decode(EncodedPrivateKey),
     PrivateKey = public_key:pem_entry_decode(PemEntry),
     Payload = <<EncodedJWTHeader/binary, ".", EncodedJWTClaimSet/binary>>,
     Signature = public_key:sign(Payload, sha256, PrivateKey),
     EncodedSignature = base64url:encode(Signature),
     <<EncodedJWTHeader/binary, ".", EncodedJWTClaimSet/binary, ".", EncodedSignature/binary>>.
