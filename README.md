#### Erlang Google OAuth 2.0 API client to generate API token for Google S2S requests

### How to compile:

`google_oauth` uses `erlang.mk` as make system. To compile

```
$ make
```

To generate release

```
$ make rel
```

### How to use with rebar:

You can use `google_oauth` as a dependency in your rebar.config:

```
{deps , [
    {google_oauth, ".*", {git, "https://github.com/pankajsoni19/google_oauth.git", {tag, "1.0.0"}}}
]}.
```

### How to run the application fcm-erlang:

`make rel` will create a release under `_rel/google_oauth` directory.

```
$ cd _rel/google_oauth
$ bin/google_oauth console
```

### Request Token

```
{ok, Result} = google_oauth:get_access_token("service_account_file_path.json", SCOPE)

Result = 
#{
        access_token    => binary()
        expires_in      => integer() :: seconds, :: < 3600
        token_type      => binary() :: <<"Bearer">>
}
```

for push notification

```
google_oauth:get_access_token("service_account_file_path.json", <<"https://www.googleapis.com/auth/firebase.messaging">>)
```
