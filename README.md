##### Google OAuth 2.0 for Server to Server Applications. This application will get a new access token for every one hour.


####### How to get the RSA Private Key file before running the application
- Reference https://developers.google.com/identity/protocols/OAuth2ServiceAccount
- after downloading the privatekey.p12 file enter the following 2 commands to convert in to RSA Private Key
- 1) openssl pkcs12 -in privatekey.p12 -out priv/temp/key-enc.pem -nodes
- 2) openssl rsa -in priv/temp/key-enc.pem -out priv/temp/RSA_Key.pem
- give the path of this RSA_KEY.pem in the RSA_KEY_PATH Macro in "oauth2_s2s.erl" module.
- Enter the ISS in the config file and feel free to change other values before running


####### how to run the application
- git clone git@github.com:RajuC/Google_OAuth_2.0.git
- cd Google_OAuth_2.0/
- make
- erl -pa ebin/ deps/*/ebin -s oauth2_s2s -config config/sys.config

- 1> oauth2_s2s:get_access_token().
- [{<<"access_token">>,
  <<"ya29.LwL5_tv6bmaZyqzCm8-LiEA27dUi5UNps5ULvVgOdD-qXvZzxKCvQicModZyqn0r6g">>},
  {<<"token_type">>,<<"Bearer">>},
 {<<"expires_in">>,3600}]


- 2> oauth2_s2s:get_access_token(<<"https://www.googleapis.com/auth/devstorage.full_control">>).
- [{<<"access_token">>,
  <<"ya29.LwJyfO4raybzP5bzDyqIf6lPHjc8rMZeZuxxjGWLy5BMw1L6PzgSvtW54fIg6jbtb">>},
  {<<"token_type">>,<<"Bearer">>},
 {<<"expires_in">>,3600}]
