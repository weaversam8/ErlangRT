-module(basic).
-export([start/0]).

start() ->
    [1,2,3,4] = test1([1, 2, 3, 4]).

test1(X) -> X.
