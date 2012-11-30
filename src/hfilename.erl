-module(hfilename).

-export([
    extension/1,
    extension/2
  ]).

extension(File) ->
  extension(File, 1).

extension(File, Num) ->
  Tokens = string:tokens(File, "."),

  case length(Tokens) of
    1 -> no_extension;
    Length -> string:join(lists:sublist(Tokens, Length + 1 - Num, Length), ".")
  end.
