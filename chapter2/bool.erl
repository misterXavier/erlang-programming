-module(bool).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

b_not(true) ->
    false;
b_not(false) ->
    true.

b_and(true, true) ->
    true;
b_and(_v1, _v2) when is_boolean(_v1) and is_boolean(_v2) ->
    false.

b_or(false, false) ->
    false;
b_or(_v1, _v2) when is_boolean(_v1) and is_boolean(_v2) ->
    true.

b_nand(v1, v2) when is_boolean(v1) and is_boolean(v2) ->
    b_not(b_and(v1, v2)).


