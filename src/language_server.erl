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
		{'initialize', Id, Args} ->
			Reply = erlang_language_server:initialize(State, Id, Args),
			reply(State#state.proxy, Id, Reply),
			loop(State)
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
			NewState = erlang_language_server:updated_configuration(TmpState, Settings),
			loop(NewState);
		{'workspace/didChangeWatchedFiles', Args} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:updated_watched_files(TmpState, Args),
			loop(NewState);
		{'textDocument/didChange', Args} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:updated_file(TmpState, Args),
			loop(NewState);
		{'textDocument/didClose', Args} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:closed_file(TmpState, Args),
			loop(NewState);
		{'textDocument/didOpen', Args} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:opened_file(TmpState, Args),
			loop(NewState);
		{'textDocument/didSave', Args} ->
			TmpState = cancel_all_pending_reads(State),
			NewState = erlang_language_server:saved_file(TmpState, Args),
			loop(NewState);

		{'workspace/symbol', Id, Args} ->
			run(Id, workspace_symbol, Args, State),
			loop(State);
		{'textDocument/completion', Id, Args} ->
			run(Id, completion, Args, State),
			loop(State);
		{'completionItem/resolve', Id, Args} ->
			run(Id, completion_resolve, Args, State),
			loop(State);
		{'textDocument/hover', Id, Args} ->
			run(Id, hover, Args, State),
			loop(State);
		{'textDocument/references', Id, Args} ->
			run(Id, references, Args, State),
			loop(State);
		{'textDocument/documentHighlight', Id, Args} ->
			run(Id, documentHighlight, Args, State),
			loop(State);
		{'textDocument/documentSymbol', Id, Args} ->
			run(Id, document_symbol, Args, State),
			loop(State);
		{'textDocument/formatting', Id, Args} ->
			run(Id, formatting, Args, State),
			loop(State);
		{'textDocument/rangeFormatting', Id, Args} ->
			run(Id, range_formatting, Args, State),
			loop(State);
		{'textDocument/onTypeFormatting', Id, Args} ->
			run(Id, on_type_formatting, Args, State),
			loop(State);
		{'textDocument/definition', Id, Args} ->
			run(Id, definition, Args, State),
			loop(State);
		{'textDocument/signatureHelp', Id, Args} ->
			run(Id, signature_help, Args, State),
			loop(State);
		{'textDocument/codeAction', Id, Args} ->
			run(Id, code_action, Args, State),
			loop(State);
		{'textDocument/codeLens', Id, Args} ->
			run(Id, code_lens, Args, State),
			loop(State);
		{'codeLens/resolve', Id, Args} ->
			run(Id, cide_lens_resolve, Args, State),
			loop(State);
		{'textDocument/rename', Id, Args} ->
			run(Id, rename, Args, State),
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

run(Id, Method, Params, State) ->
	Self = self(),
	spawn(fun() ->
				  %% TODO make it cancellable (and with possible partial results)
				  Internal = State#state.internal_state,
				  Result = erlang_language_server:Method(Params, Internal),
				  reply(State#state.proxy, Id, Result)
		  end).


