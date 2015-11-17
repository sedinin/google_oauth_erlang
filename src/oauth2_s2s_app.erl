-module(oauth2_s2s_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	oauth2_s2s_sup:start_link().

stop(_State) ->
	ok.
