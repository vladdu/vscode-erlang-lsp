-module(erlang_language_server).

-export([
		 init/0,
		 initialize/2,
		 updated_configuration/2,
		 updated_watched_files/2,
		 changed_file/3,
		 opened_file/2,
		 closed_file/2,
		 saved_file/2,
		 
		 workspace_symbol/2,
		 completion/2,
		 completion_resolve/2,
		 hover/2,
		 references/2,
		 document_highlight/2,
		 document_symbol/2,
		 formatting/2,
		 range_formatting/2,
		 on_type_formatting/2,
		 definition/2,
		 signature_help/2,
		 code_action/2,
		 code_lens/2,
		 code_lens_resolve/2,
		 rename/2
		]).

-record(state, {
				client_capabilities = #{},
				server_capabilities = #{},
				configuration = #{},
				watched_files = [],
				open_files = []
			   }).

init() ->
	#state{}.

initialize(State, ClientCapabilities) ->
	Capabilities = #{
					 textDocumentSync => 1, %% 0=none, 1=full, 2=incremental
					 hoverProvider => true,
					 completionProvider => #{
											 resolveProvider => true,
											 triggerCharacters => []
											},
					 signatureHelpProvider => #{
												triggerCharacters => []
											   },
					 definitionProvider => true,
					 referencesProvider => true,
					 documentHighlightProvider => true,
					 documentSymbolProvider => true,
					 workspaceSymbolProvider => true,
					 codeActionProvider => true,
					 codeLensProvider => #{
										   resolveProvider => true
										  },
					 documentFormattingProvider => true,
					 documentRangeFormattingProvider => true,
					 documentOnTypeFormattingProvider => #{
														   firstTriggerCharacter => <<"">>,
														   moreTriggerCharacters => []
														  },
					 renameProvider => true
					},
	Server = #{capabilities => Capabilities},
	{Server, State#state{client_capabilities=ClientCapabilities, server_capabilities=Server}}.

updated_configuration(State, Settings) ->
	State#state{configuration=Settings}.

updated_watched_files(State, _Changes) ->
	Watched = State#state.watched_files,
	NewWatched = lists:foldl(fun process_watched/2, [], Watched),
	State#state{watched_files=NewWatched}.

opened_file(State, #{uri:=URI, languageId:=_Language, version:=_Version, text:=_Text}=Item) ->
	Open = State#state.open_files,
	NewOpen = [{URI, Item}|Open],
	State#state{open_files=NewOpen}.

changed_file(State, #{uri:=_URI, version:=_Version}, _Changes) ->
	%% TODO start parsing & processing
	State.

saved_file(State, #{uri:=_URI}) ->
	State.

closed_file(State, #{uri:=URI}) ->
	Open = State#state.open_files,
	NewOpen = lists:keydelete(URI, 1, Open),
	State#state{open_files=NewOpen}.

workspace_symbol(_State, _Args) ->
	ok.

%% -type completion_item() :: #{'label'=>string(), 'kind'=>integer(), ...}.

completion(_State, {_DocumentId, _Position}) ->
	#{
	  isIncomplete=>false,
	  items => []
	 }.

completion_resolve(_State, Item) ->
	Item.

hover(_State, {_DocumentId, _Position}) ->
	%% [markedstring()]:: String (=markdown), #{language, value}
	#{
	  contents => []
	 %%, range => language_server_utils:range(_Position, _Position)
	 }.

references(_State, _Args) ->
	ok.

document_highlight(_State, _Args) ->
	ok.

document_symbol(_State, _Args) ->
	ok.

formatting(_State, _Args) ->
	ok.

range_formatting(_State, _Args) ->
	ok.

on_type_formatting(_State, _Args) ->
	ok.

definition(_State, _Args) ->
	ok.

signature_help(_State, _Args) ->
	ok.

code_action(_State, _Args) ->
	ok.

code_lens(_State, _Args) ->
	ok.

code_lens_resolve(_State, _Args) ->
	ok.

rename(_State, _Args) ->
	ok.


%%%%%%%%%%%%%%%%%

process_watched(#{uri:=URI, type:=1}, List) ->
	%% TODO start parsing & processing
	[URI|List];
process_watched(#{uri:=_URI, type:=2}, List) ->
	%% TODO start parsing & processing
	List;
process_watched(#{uri:=URI, type:=3}, List) ->
	lists:delete(URI, List).
