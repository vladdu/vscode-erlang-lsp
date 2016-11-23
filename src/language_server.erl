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
				crt_id = 0,
				pending_reads = [],
				pending_writes = [],
				pending_requests = [],
				internal_state
			   }).

start([Port0]) ->
	Port = list_to_integer(atom_to_list(Port0)),
	erlang:register(?SERVER, self()),
	Proxy = spawn_link(fun() ->
							   language_server_proxy:start([?SERVER, Port])
					   end),
	State = #state{proxy = Proxy,
				   internal_state = erlang_language_server:init()
				  },
	init(State).

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

init(State) ->
	receive
		{'initialize', Id, ClientCapabilities} ->
			{ServerCapabilities, NewState} = erlang_language_server:initialize(State#state.internal_state, ClientCapabilities),
			reply(State#state.proxy, Id, ServerCapabilities),
			loop(State#state{internal_state=NewState})
	end.


loop(#state{stopped = true}) ->
	receive
		{'exit', _} ->
			erlang:halt()
	end;
loop(State = #state{proxy = Proxy}) ->
	receive
		{'shutdown', _Id, _} ->
			loop(State#state{stopped = true});
		{'exit', _} ->
			erlang:halt();

		{'$/cancelRequest', #{id := Id}} ->
			NewState = cancel_read(Id, State),
			loop(NewState);
		{'workspace/didChangeConfiguration', #{settings := Settings}} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:updated_configuration(TmpState#state.internal_state, Settings),
			loop(TmpState#state{internal_state=NewState});
		{'workspace/didChangeWatchedFiles', #{changes := Changes}} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:updated_watched_files(TmpState#state.internal_state, Changes),
			loop(TmpState#state{internal_state=NewState});
		{'textDocument/didOpen', #{textDocument := Document}} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:opened_file(TmpState#state.internal_state, Document),
			loop(TmpState#state{internal_state=NewState});
		{'textDocument/didChange', #{textDocument := VersionedDocument, contentChanges := Changes}} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:changed_file(TmpState#state.internal_state, VersionedDocument, Changes),
			loop(TmpState#state{internal_state=NewState});
		{'textDocument/didSave', #{textDocument := DocumentId}} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:saved_file(TmpState#state.internal_state, DocumentId),
			loop(TmpState#state{internal_state=NewState});
		{'textDocument/didClose', #{textDocument := DocumentId}} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:closed_file(TmpState#state.internal_state, DocumentId),
			loop(TmpState#state{internal_state=NewState});

		{'workspace/symbol', Id, #{query:=Query}} ->
			Result = case start_worker(Id, workspace_symbol, Query, State) of
						 nil ->
							 [];
						 R ->
							 R
					 end,
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/completion', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Result = start_worker(Id, completion, {URI, Position}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'completionItem/resolve', Id, CompletionItem} ->
			Result = start_worker(Id, completion_resolve, CompletionItem, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/hover', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Result = start_worker(Id, hover, {URI, Position}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/references', Id, #{textDocument:=#{uri:=URI}, position:=Position, context:=Context}} ->
			Result = start_worker(Id, references, {URI, Position, Context}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/documentHighlight', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Result = start_worker(Id, document_highlight, {URI, Position}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/documentSymbol', Id, #{textDocument:=#{uri:=URI}}} ->
			Result = start_worker(Id, document_symbol, URI, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/formatting', Id,  #{textDocument:=#{uri:=URI}, options:=Options}} ->
			Result = start_worker(Id, formatting, {URI, Options}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/rangeFormatting', Id, #{textDocument:=#{uri:=URI}, range:=Range, options:=Options}} ->
			Result = start_worker(Id, range_formatting, {URI, Range, Options}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/onTypeFormatting', Id, #{textDocument:=#{uri:=URI}, position:=Position, ch:=Ch, options:=Options}} ->
			Result = start_worker(Id, on_type_formatting, {URI, Position, Ch, Options}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/definition', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Result = start_worker(Id, definition, {URI, Position}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/signatureHelp', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Result = start_worker(Id, signature_help, {URI, Position}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/codeAction', Id, #{textDocument:=#{uri:=URI}, range:=Range, context:=Context}} ->
			Result = start_worker(Id, code_action, {URI, Range, Context}, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/codeLens', Id, #{textDocument:=#{uri:=URI}}} ->
			Result = start_worker(Id, code_lens, URI, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'codeLens/resolve', Id, Item} ->
			Result = start_worker(Id, code_lens_resolve, Item, State),
			reply(Proxy, Id, Result),
			loop(State);
		{'textDocument/rename', Id, #{textDocument:=#{uri:=URI}, position:=Position, newName:=NewName}} ->
			Result = start_worker(Id, rename, {URI, Position, NewName}, State),
			reply(Proxy, Id, Result),
			loop(State);

		{show_message, Type, Msg} ->
			Proxy ! {notify, 'window/showMessage',
					 #{type => Type,
					   message => iolist_to_binary(Msg)}},
			loop(State);
		{show_message_request, Type, Msg, Actions, Pid} ->
			Id = State#state.crt_id,
			NewState = State#state{
								   pending_requests = [{Id, Pid} | State#state.pending_requests],
								   crt_id = Id + 1
								  },
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
			case lists:keytake(Id, 1, State#state.pending_requests) of
				false ->
					loop(State);
				{value, {Id, Pid}, Rest} ->
					Pid ! Msg,
					loop(State#state{pending_requests=Rest})
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

reply(Proxy, Id, Answer) ->
	Proxy ! {reply, Id, Answer}.


cancel_read(Id, #state{pending_reads=Reqs}=State) ->
	case lists:keytake(Id, 1, Reqs) of
		{value, {Id, Pid}, NewReqs} ->
			Pid ! cancel,
			Answer = receive
						 X ->
							 X
					 after 5000 ->
						 {error, internal_error, <<"operation timeout">>}
					 end,
			reply(State#state.proxy, Id, Answer),
			State#state{pending_reads=NewReqs};
		false ->
			State
	end.

cancel_all_pending_reads(#state{pending_reads=Reqs}=State) ->
	[cancel_read(X, State) || {X, _} <- Reqs],
	State#state{pending_reads=[]}.

start_worker(Id, Method, Params, State) ->
	spawn(fun() ->
				  Fun = fun(Reporter) ->
								Internal = State#state.internal_state,
								erlang_language_server:Method(Internal, Params, Reporter)
						end,
				  {ok, Key} = cancellable_worker:start(Fun),
				  {ok, Result} = cancellable_worker:yield(Key),
				  reply(State#state.proxy, Id, Result)
		  end).
