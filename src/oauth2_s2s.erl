-module(oauth2_s2s).
-export([start/0,get_access_token/0,get_access_token/1]).


-include_lib("public_key/include/public_key.hrl").
%%path of the RSA key file , check READ me for more info
-define(RSA_KEY_PATH,"oauth_test-6aaebb132d95_RSA.pem").
-define(JWT_HEADER,[{alg, <<"RS256">>}, {typ, <<"JWT">>}]).

start() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok =application:start(ssl),
    ok = application:start(inets),
    ok = application:start(jsx),
    ok = application:start(oauth2_s2s),
    io:format("~n~nstarted oauth2_s2s....~n~n ").


%% access token for pubsub api (default here as present in the config file)
get_access_token() ->
  {ok, Scope} = application:get_env(oauth2_s2s, scope),
  access_token(Scope).

 get_access_token(Scope) ->
    access_token(Scope).

access_token(Scope) ->
  {ok, Host} = application:get_env(oauth2_s2s, host),
  {ok, Aud} = application:get_env(oauth2_s2s, aud),
  {ok, Iss} = application:get_env(oauth2_s2s,iss),
  {ok, GrantType} = application:get_env(oauth2_s2s, grant_type),
    {ok,EncodedPrivateKey1} = file:read_file(?RSA_KEY_PATH),
    [PemEntry] = public_key:pem_decode(EncodedPrivateKey1),
    PrivateKey = public_key:pem_entry_decode(PemEntry),
    EncodedJWTHeader = encode_base64(?JWT_HEADER),
    EncodedJWTClaimSet = encode_base64(jwt_claim_set(Iss, Scope, Aud)),
    Signature = compute_signature(EncodedJWTHeader, EncodedJWTClaimSet, PrivateKey),
    Jwt = binary:replace(
        binary:replace(<<EncodedJWTHeader/binary, ".", EncodedJWTClaimSet/binary, ".", Signature/binary>>,
                     <<"+">>, <<"-">>, [global]),
        <<"/">>, <<"_">>, [global]),
%%    io:format("Jwt::~p~n",[Jwt]),
    make_http_req(post,Host,
                    "application/x-www-form-urlencoded",
                    <<"grant_type=",GrantType/binary,"&assertion=",Jwt/binary>>).

encode_base64(Json) ->
    base64:encode(jsx:encode(Json)).

jwt_claim_set(Iss, Scope, Aud) ->
    [{iss, Iss},
     {scope, Scope},
     {aud, Aud},
     {exp, calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 62167219200 + 3600},
     {iat, calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 62167219200}].

compute_signature(Header, ClaimSet, #'RSAPrivateKey'{publicExponent=Exponent,
                                                    modulus=Modulus,
                                                    privateExponent=PrivateExponent}) ->
    base64:encode(crypto:sign(rsa, sha256, <<Header/binary, ".", ClaimSet/binary>>, 
                                [Exponent, Modulus, PrivateExponent])).


make_http_req(Method, Url, ContType, Body) ->
case httpc:request(Method, {binary_to_list(Url), [], ContType, Body},[],[]) of
    {ok, {{"HTTP/1.1",200, _State}, _Head, ResponseBody}} ->
%%        io:format("Body :~p~n~n",[ResponseBody]),
        jsx:decode(list_to_binary(ResponseBody));
    {ok, {{"HTTP/1.1",_ResponseCode, _State}, _Head, ResponseBody}} ->
%%        io:format("Response code : ~p~n Body :~p~n~n",[ResponseCode, ResponseBody]),
         jsx:decode(list_to_binary(ResponseBody));
    {error,Reason} ->
        io:format("~nError Resason : ~p~n",[Reason]),
        {error,Reason}
  end.

