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
	process_flag(trap_exit, true),
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
loop(State = #state{proxy = Proxy, pending_reads=Reqs}) ->
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
			Pid = start_worker(Id, workspace_symbol, Query, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/completion', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Pid = start_worker(Id, completion, {URI, Position}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'completionItem/resolve', Id, CompletionItem} ->
			Pid = start_worker(Id, completion_resolve, CompletionItem, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/hover', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Pid = start_worker(Id, hover, {URI, Position}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/references', Id, #{textDocument:=#{uri:=URI}, position:=Position, context:=Context}} ->
			Pid = start_worker(Id, references, {URI, Position, Context}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/documentHighlight', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Pid = start_worker(Id, document_highlight, {URI, Position}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/documentSymbol', Id, #{textDocument:=#{uri:=URI}}} ->
			Pid = start_worker(Id, document_symbol, URI, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/formatting', Id,  #{textDocument:=#{uri:=URI}, options:=Options}} ->
			Pid = start_worker(Id, formatting, {URI, Options}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/rangeFormatting', Id, #{textDocument:=#{uri:=URI}, range:=Range, options:=Options}} ->
			Pid = start_worker(Id, range_formatting, {URI, Range, Options}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/onTypeFormatting', Id, #{textDocument:=#{uri:=URI}, position:=Position, ch:=Ch, options:=Options}} ->
			Pid = start_worker(Id, on_type_formatting, {URI, Position, Ch, Options}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/definition', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Pid = start_worker(Id, definition, {URI, Position}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/signatureHelp', Id, #{textDocument:=#{uri:=URI}, position:=Position}} ->
			Pid = start_worker(Id, signature_help, {URI, Position}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/codeAction', Id, #{textDocument:=#{uri:=URI}, range:=Range, context:=Context}} ->
			Pid = start_worker(Id, code_action, {URI, Range, Context}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/codeLens', Id, #{textDocument:=#{uri:=URI}}} ->
			Pid = start_worker(Id, code_lens, URI, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'codeLens/resolve', Id, Item} ->
			Pid = start_worker(Id, code_lens_resolve, Item, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});
		{'textDocument/rename', Id, #{textDocument:=#{uri:=URI}, position:=Position, newName:=NewName}} ->
			Pid = start_worker(Id, rename, {URI, Position, NewName}, State),
			NewReqs = [{Id, Pid}|Reqs],
			loop(State#state{pending_reads=NewReqs});

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

		{_F, _A})=Other ->
			%% unknown notification, ignore
			io:format("Unrecognized notification  ~p~n", [Other]),
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

reply(Proxy, Id, undefined, DefaultAnswer) ->
	reply(Proxy, Id, DefaultAnswer);
reply(Proxy, Id, Answer, _DefaultAnswer) ->
	Proxy ! {reply, Id, Answer}.

cancel_read(Id, #state{pending_reads=Reqs}=State) ->
	case lists:keytake(Id, 1, Reqs) of
		{value, {Id, Pid}, NewReqs} ->
			Pid ! cancel,
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
				  {ok, MonPid} = cancellable_worker:start(Fun),
				  DfltAnswer = erlang_language_server:default_answer(Method),
				  my_worker_loop(Id, State, MonPid, DfltAnswer)
		  end).

my_worker_loop(Id, State, MonPid, DfltAnswer) ->
	receive
		cancel ->
			{_, Result} = cancellable_worker:cancel(MonPid),
			reply(State#state.proxy, Id, Result, DfltAnswer)
	after 10 ->
		case cancellable_worker:check(MonPid) of
			{partial, _} ->
				my_worker_loop(Id, State, MonPid, DfltAnswer);
			{final, Result} ->
				reply(State#state.proxy, Id, Result)
		end
	end.

