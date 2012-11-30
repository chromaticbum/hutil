-module(hless).

-export([
    start/0
  ]).

start() ->
  {ok, LessVm} = js_driver:new(),

  LessJs = filename:join(filename:absname(code:priv_dir(hutil)), "less.js"),
  js_driver:define_js(LessVm, <<"window = {};">>),
  js_driver:define_js(LessVm, {file, LessJs}),

  Result = js:call(LessVm, <<"compile">>, [<<".class { width: (1 + 1) }">>]),
  io:format("HEHEHEHE ~p~n", [Result]),

  application:set_env(hutil, less_vm, LessVm),

  ok.
