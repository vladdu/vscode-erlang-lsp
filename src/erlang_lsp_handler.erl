-module(erlang_lsp_handler).

-export([
		 initialize/1,

		 '$handle_undefined_function'/2
		]).


initialize(_ServerCapabilities) ->
	Capabilities = #{
					 <<"textDocumentSync">> => 2,
					 <<"hoverProvider">> => true,
					 <<"documentHighlightProvider">> => true
					},
	#{<<"capabilities">> => Capabilities}.

'$handle_undefined_function'(F, [A]) ->
	io:format("CALLED ~p ~p~n", [F, A]),
	#{}.