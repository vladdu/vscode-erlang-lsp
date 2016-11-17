-module(erlang_lsp_app).

-behaviour(application).
-export([start/2, stop/1]).

-export([]).

start(_Type, _StartArgs) ->
	ok.

stop(_State) ->
	ok.

%% ======================
%% Internal functions
%% ======================


