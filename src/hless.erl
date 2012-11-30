-module(hless).

-export([
    start/0
  ]).

start() ->
  {ok, LessJs} = js_driver:new(),
  application:set_env(hutil, less_js, LessJs),

  ok.
