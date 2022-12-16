-module(hello).
-export([hello_world/0]).

hello_world()->
    io:format("Hello World ~n").

%-----------------------------------------------------------------------
%And Executing:
%
%hello:hello_world().
