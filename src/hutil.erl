-module(hutil).

-export([
    start/0
  ]).

start() ->
  application:start(sasl),
  application:start(erlang_js),
  application:start(hutil).
