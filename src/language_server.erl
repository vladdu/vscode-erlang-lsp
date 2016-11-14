%% @author vlad
%% @doc @todo Add description to language_server.

-module(language_server).

-export([
		 start/1,

		 show_message/2,
		 show_message_request/3,
		 log_message/2,
		 telemetry_event/1,
		 publish_diagnostics/2
		]).

-define(SERVER, ?MODULE).

-record(state, {
				proxy,
				stopped = false,
				internal = #{},
				requests = []
			   }).

start([Port0]) ->
	Port = list_to_integer(atom_to_list(Port0)),
	erlang:register(?SERVER, self()),
	Proxy = spawn_link(fun() -> language_server_proxy:start([?SERVER, Port]) end),
	State = #state{proxy=Proxy},
	loop(State).

%% client API

show_message(Type, Msg) ->
	?SERVER ! {show_message, Type, Msg}.

show_message_request(Type, Msg, Actions) ->
	?SERVER ! {show_message_request, Type, Msg, Actions, self()},
	receive
		Msg ->
			Msg
	end.

log_message(Type, Msg) ->
	?SERVER ! {log_message, Type, Msg}.

telemetry_event(Msg) ->
	?SERVER ! {telemetry_event, Msg}.

publish_diagnostics(URI, Diagnostics) ->
	?SERVER ! {publish_diagnostics, URI, Diagnostics}.

%%%%%%%%%%%%%%%%%%%%%

loop(#state{stopped=true}) ->
	receive
		'exit' ->
			erlang:halt()
	end;
loop(State=#state{proxy=Proxy}) ->
	receive
		{'initialize', Id, Args} ->
			NewState = initialize(State, Id, Args),
			loop(NewState);
		{'shutdown', _Id, _} ->
			loop(State#state{stopped=true});
		{'exit', _} ->
			erlang:halt();
		{'$/cancelRequest', #{id:=Id}} ->
			cancel_request(Id),
			loop(State);
		{'workspace/didChangeConfiguration', #{settings := Settings}} ->
			NewState = update_configuration(State, Settings),
			loop(NewState);
		{'workspace/didChangeWatchedFiles', Args} ->
			loop(State);
		{'workspace/symbol', Id, Args} ->
			loop(State);
		{'textDocument/didChange', Args} ->
			loop(State);
		{'textDocument/didClose', Args} ->
			loop(State);
		{'textDocument/didOpen', Args} ->
			loop(State);
		{'textDocument/didSave', Args} ->
			loop(State);
		{'textDocument/completion', Id, Args} ->
			loop(State);
		{'completionItem/resolve', Id, Args} ->
			loop(State);
		{'textDocument/hover', Id, Args} ->
			loop(State);
		{'textDocument/references', Id, Args} ->
			loop(State);
		{'textDocument/documentHighlight', Id, Args} ->
			loop(State);
		{'textDocument/documentSymbol', Id, Args} ->
			loop(State);
		{'textDocument/formatting', Id, Args} ->
			loop(State);
		{'textDocument/rangeFormatting', Id, Args} ->
			loop(State);
		{'textDocument/onTypeFormatting', Id, Args} ->
			loop(State);
		{'textDocument/definition', Id, Args} ->
			loop(State);
		{'textDocument/signatureHelp', Id, Args} ->
			loop(State);
		{'textDocument/codeAction', Id, Args} ->
			loop(State);
		{'textDocument/codeLens', Id, Args} ->
			loop(State);
		{'codeLens/resolve', Id, Args} ->
			loop(State);
		{'textDocument/rename', Id, Args} ->
			loop(State);

		{show_message, Type, Msg} ->
			Proxy ! {notify, 'window/showMessage',
					 #{type => Type,
					   message => iolist_to_binary(Msg)}},
			loop(State);
		{show_message_request, Type, Msg, Actions, Pid} ->
			Id = erlang:unique_integer(),
			NewState = State#state{requests=[{Id, Pid}|State#state.requests]},
			Proxy ! {request, Id, 'window/showMessageRequest',
					 #{type => Type,
					   message => iolist_to_binary(Msg),
					   actions => Actions}
					},
			loop(NewState);
		{log_message, Type, Msg} ->
			Proxy ! {notify, 'window/logMessage',
					 #{type => Type,
					   message => iolist_to_binary(Msg)}},
			loop(State);
		{telemetry_event, Msg} ->
			Proxy ! {notify, 'telemetry/event', Msg},
			loop(State);
		{publish_diagnostics, URI, Diagnostics} ->
			Proxy ! {notify, 'textDocument/publishDiagnostics',
					 #{uri => URI,
					   diagnostics => Diagnostics}},
			loop(State);

		{'$reply', Id, Msg} ->
			case lists:keytake(Id, 1, State#state.requests) of
				false ->
					loop(State);
				{value, {Id, Pid}, Rest} ->
					Pid ! Msg,
					loop(State#state{requests=Rest})
			end;

		{_F, _A} ->
			%% unknown notification, ignore
			loop(State);
		{F, Id, A} ->
			FN = atom_to_binary(F, latin1),
			AN = iolist_to_binary(io_lib:format("~p~n", [A])),
			reply(State, Id, #{error => #{code => method_not_found,
										  message => <<"Unrecognized method ", FN/binary,
													   " called with ", AN/binary>>}}),
			loop(State);
		Other ->
			io:format("Unrecognized message  ~p~n", [Other]),
			loop(State)
	end.

reply(State, Id, Answer) ->
	Proxy = State#state.proxy,
	Proxy ! {reply, Id, Answer}.

initialize(State, Id, ClientCapabilities) ->
	ServerCapabilities = initialize(),
	reply(State, Id, ServerCapabilities),
	I = State#state.internal,
	State#state{internal = I#{
							  client_capabilities => ClientCapabilities,
							  server_capabilities => ServerCapabilities
							 }
			   }.

initialize() ->
	CompletionOptions = #{
						  },
	SignatureHelpOptions = #{
							 },
	% CodeLensOptions = #{},
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
					 % codeLensProvider => CodeLensOptions,
					 documentFormattingProvider => true,
					 documentRangeFormattingProvider => true,
					 documentOnTypeFormattingProvider => DocumentOnTypeFormattingOptions,
					 renameProvider => true
					},
	#{capabilities => Capabilities}.

cancel_request(_Id) ->
	ok.

update_configuration(State, _Settings) ->
	%% TODO implement
	State.
