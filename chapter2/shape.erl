-module(shape).
-export([area/1]).

% This is a comment

area({square, Side}) ->
    Side * Side;
area({circle, Radius}) ->
    math:pi() * Radius * Radius;
area(_Other) ->
    {error, invalid_object}.
