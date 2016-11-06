-module(erlang_lsp).

-export([
		 start/1,
		 loop/4
		]).

start([Port0]) ->
	Port = list_to_integer(atom_to_list(Port0)),
	run([{port, Port}]).

-define(TCP_OPTIONS,[binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

run(Opts) ->
	Port = proplists:get_value(port, Opts),
	{ok, Socket} = gen_tcp:connect("localhost", Port, ?TCP_OPTIONS),
	loop(Socket, Opts).

loop(Socket, Opts) ->  
	loop(Socket, Opts, <<"">>, 0).

loop(Socket, Opts, Buf, N) ->
	case erlang:decode_packet(httph, Buf, []) of
		{ok, {http_header, _, 'Content-Length', undefined, Len}, R} ->
			?MODULE:loop(Socket, Opts, R, list_to_integer(Len));
		{ok, {http_header, _, _, _, _}, R} ->
			?MODULE:loop(Socket, Opts, R, N);
		{ok, http_eoh, R} ->
			{ok, New}  = if size(R) < N ->
								gen_tcp:recv(Socket, N - size(R));
							true ->
								{ok, <<"">>}
						 end,
			<<D:N/binary, Rest/binary>> = <<R/binary, New/binary>>,
			process_request(Socket, D),
			?MODULE:loop(Socket, Opts, Rest, 0);
		{more, undefined} ->
			{ok, MData} = gen_tcp:recv(Socket, 0),
			?MODULE:loop(Socket, Opts, <<Buf/binary, MData/binary>>, N);
		{more, Len} ->
			{ok, MData} = gen_tcp:recv(Socket, Len),
			?MODULE:loop(Socket, Opts, <<Buf/binary, MData/binary>>, N)
	end.

process_request(Socket, D) ->
	Req = parse(D),
	{Id, Answer} = dispatch(Req),
	answer(Socket, Id, Answer).

parse(Data) ->
	Req = jsx:decode(Data, [return_maps]),
	io:format(">>> ~p~n", [Req]),
	Req.

dispatch(#{<<"jsonrpc">> := <<"2.0">>,
		   <<"id">> := Id,
			<<"method">> := Method0,
			<<"params">> := Params
		   }) ->
	Method = binary_to_atom(Method0, unicode),
	io:format("###  ~p ~p~n", [Method, Params]),
	{Id, erlang_lsp_handler:Method(Params)};
dispatch(#{<<"jsonrpc">> := <<"2.0">>,
			<<"method">> := Method0,
			<<"params">> := Params
		   }) ->
	Method = binary_to_atom(Method0, unicode),
	io:format("###  ~p ~p~n", [Method, Params]),
	{-1, erlang_lsp_handler:Method(Params)}.

answer(_Socket, -1, _Msg) when is_map(_Msg) ->
	ok;
answer(Socket, Id, Msg) when is_map(Msg) ->
	io:format("REPLY ~p~n", [Msg]),
	Ans0 = #{<<"jsonrpc">> => <<"2.0">>,
			<<"result">> => Msg
		   },
	Ans = if Id >= 0 -> Ans0#{<<"id">>=>Id}; true -> Ans0 end,
	Json = jsx:encode(Ans),
	Hdr = io_lib:format("Content-Length: ~w\r\n\r\n", [size(Json)]),
	io:format("WRITE ~s~s~n", [Hdr, Json]),
	gen_tcp:send(Socket, Hdr),
	gen_tcp:send(Socket, Json).
