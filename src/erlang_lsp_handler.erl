-module(erlang_lsp_handler).

-export([
		 method_to_function/1,
		 function_to_method/1,

		 initialize/1,
		 shutdown/0,
		 exit/0,

		 on_show_message/1,
		 on_log_message/1,
		 on_telemetry/1,
		 on_publish_diagnostics/1,

		 did_change_configuration/1,
		 did_change_document/1,
		 did_change_watched_files/1,
		 did_close_document/1,
		 did_open_document/1,
		 did_save_document/1,

		 completion/1,
		 completion_item_resolve/1,
		 hover/1,
		 find_references/1,
		 document_highlights/1,
		 document_symbols/1,
		 format/1,
		 format_on_type/1,
		 goto_definition/1,
		 signature_help/1,
		 code_action/1,
		 code_lens/1,
		 workspace_symbols/1,
		 rename/1,

		 '$handle_undefined_function'/2
		]).

-inline([methods/0]).

methods() ->
	[
	 {'initialize', initialize},
	 {'shutdown', shutdown},
	 {'exit', exit},

	 {'window/showMessage', on_show_message},
	 {'window/logMessage', on_log_message},
	 {'telemetry/event', on_telemetry},
	 {'textDocument/publishDiagnostics', on_publish_diagnostics},

	 {'workspace/didChangeConfiguration', did_change_configuration},
	 {'textDocument/didChange', did_change_document},
	 {'workspace/didChangeWatchedFiles', did_change_watched_files},
	 {'textDocument/didClose', did_close_document},
	 {'textDocument/didOpen', did_open_document},
	 {'textDocument/didSave', did_save_document},

	 {'textDocument/completion', completion},
	 {'completionItem/resolve', completion_item_resolve},
	 {'textDocument/hover', hover},
	 {'textDocument/references', find_references},
	 {'textDocument/documentHighlight', document_highlights},
	 {'textDocument/documentSymbol', document_symbols},
	 {'textDocument/formatting', format},
	 {'textDocument/onTypeFormatting', format_on_type},
	 {'textDocument/definition', goto_definition},
	 {'textDocument/signatureHelp', signature_help},
	 {'textDocument/codeAction', code_action},
	 {'textDocument/codeLens', code_lens},
	 {'workspace/symbol', workspace_symbols},
	 {'', rename}
	].

%% TODO there a re a few more methods!

%% TODO notifications vs requests

method_to_function(M) ->
	case lists:keyfind(M, 1, methods()) of
		{M, V} -> V;
		false -> M
	end.

function_to_method(M) ->
	case lists:keyfind(M, 2, methods()) of
		{V, M} -> V;
		false -> M
	end.

initialize(_ServerCapabilities) ->
	CompletionOptions = #{
						  },
	SignatureHelpOptions = #{
							 },
	CodeLensOptions = #{
						},
	DocumentOnTypeFormattingOptions = #{
										},
	Capabilities = #{
					 <<"textDocumentSync">> => 2,
					 <<"hoverProvider">> => false,
					 %% 					 <<"completionProvider">> => CompletionOptions,
					 %% 					 <<"signatureHelpProvider">> => SignatureHelpOptions,
					 <<"definitionProvider">> => false,
					 <<"referencesProvider">> => false,
					 <<"documentHighlightProvider">> => false,
					 <<"documentSymbolProvider">> => false,
					 <<"workspaceSymbolProvider">> => false,
					 <<"codeActionProvider">> => false,
					 %% 					 <<"codeLensProvider">> => CodeLensOptions,
					 <<"documentFormattingProvider">> => false,
					 <<"documentRangeFormattingProvider">> => false,
					 %% 					 <<"documentOnTypeFormattingProvider">> => DocumentOnTypeFormattingOptions,
					 <<"renameProvider">> => false
					},
	#{<<"capabilities">> => Capabilities}.

'$handle_undefined_function'(F, [A]) ->
	io:format("UNKNOWN ~p ~p~n", [F, A]),
	ok.

shutdown() ->
	gen_server:cast(shutdown).

exit() ->
	gen_server:cast(exit).

on_show_message(_CB) ->
	#{}.

on_log_message(_CB) ->
	#{}.

on_telemetry(_CB) ->
	#{}.

did_change_configuration(_Settings) ->
	#{}.

did_open_document(_OpenDocument) ->
	#{}.

did_change_document(_ChangeDocument) ->
	#{}.

did_close_document(_TextDocument) ->
	#{}.

did_save_document(_TextDocument) ->
	#{}.

did_change_watched_files(_Changes) ->
	#{}.

on_publish_diagnostics(_Diagnostics) ->
	#{}.

completion(_TextDocumentPosition) ->
	#{}.

completion_item_resolve(_CompletionItem) ->
	#{}.

hover(_TextDocumentPosition) ->
	#{}.

signature_help(_TextDocumentPosition) ->
	#{}.

goto_definition(_TextDocumentPosition) ->
	#{}.

find_references(_References) ->
	#{}.

document_highlights(_TextDocumentPosition) ->
	#{}.

document_symbols(_TextDocument) ->
	#{}.

workspace_symbols(_Query) ->
	#{}.

code_action(_CodeAction) ->
	#{}.

code_lens(_TextDocument) ->
	#{}.

format(_Format) ->
	#{}.

format_on_type(_Format) ->
	#{}.

rename(_Rename) ->
	#{}.
