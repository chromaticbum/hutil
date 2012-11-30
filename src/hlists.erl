-module(hlists).

-export([
    foldl/3
  ]).

foldl(_Fun, Acc, []) ->
  Acc;
foldl(Fun, Acc, [Item | Items]) ->
  case Fun(Item, Acc) of
    {next, Acc2} -> foldl(Fun, Acc2, Items);
    {stop, Acc2} -> Acc2
  end.
