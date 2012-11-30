-module(hutil_app).
-behavior(application).

-export([
    start/2,
    stop/1
  ]).

start(_Type, _Args) ->
  ok = hless:start(),
  
  {ok, self()}.

stop(_State) ->
  ok.
