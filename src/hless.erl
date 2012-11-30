-module(hless).

-export([
    compile/1
  ]).

compile(Contents) ->
  hless_srv:compile(Contents).
