-module(erlang_language_server).

-export([
		 init/0,
		 initialize/3,
		 updated_configuration/2,
		 updated_watched_files/2,
		 updated_file/2,
		 opened_file/2,
		 closed_file/2,
		 saved_file/2,

		 '$handle_undefined_function'/2
		 ]).

-record(state, {
				client_capabilities = #{},
				server_capabilities = #{},
				watched_files = [],
				open_files = []
				}).

init() ->
	#state{}.

initialize(State, Id, ClientCapabilities) ->
	CompletionOptions = #{
						  },
	SignatureHelpOptions = #{
							 },
	CodeLensOptions = #{resolveProvider => false},
	DocumentOnTypeFormattingOptions = #{
										},
	Capabilities = #{
					 textDocumentSync => 2,
					 hoverProvider => true,
					 completionProvider => CompletionOptions,
					 signatureHelpProvider => SignatureHelpOptions,
					 definitionProvider => true,
					 referencesProvider => true,
					 documentHighlightProvider => true,
					 documentSymbolProvider => true,
					 workspaceSymbolProvider => true,
					 % codeActionProvider => true,
					 codeLensProvider => CodeLensOptions,
					 documentFormattingProvider => true,
					 documentRangeFormattingProvider => true,
					 documentOnTypeFormattingProvider => DocumentOnTypeFormattingOptions,
					 renameProvider => true
					},
	#{capabilities => Capabilities}.

updated_configuration(State, _Settings) ->
	State.

updated_watched_files(State, _Settings) ->
	State.

updated_file(State, _Settings) ->
	State.

opened_file(State, _Settings) ->
	State.

saved_file(State, _Settings) ->
	State.

closed_file(State, _Settings) ->
	State.

%% FIXME temporary
'$handle_undefined_function'(F, A) ->
	io:format("@@@@ UNDEFINED ~p~n", [F]),
	#{}.