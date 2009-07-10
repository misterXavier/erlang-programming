-module(demo).
-export([double/1]).

% This is a comment

double(Value) ->
    times(Value, 2).

times(X, Y) ->
    X * Y.
