-module(cancellable_worker_tests).

-include_lib("eunit/include/eunit.hrl").

run_test(Fun, Test) ->
	{ok, Worker} = cancellable_worker:start(Fun),
	Result = (catch Test(Worker)),
	Result.

run_test_() ->
	[
	 ?_assertMatch({final, undefined},
				   run_test(fun(_M)-> ok end,
							fun(W) -> cancellable_worker:check(W) end
						   )
	 ),
	 ?_assertMatch({final, [v2]},
				   run_test(fun(M) -> M(partial, v2), ok end,
							fun(W) -> w(10), cancellable_worker:check(W) end
						   )
	 ),
	 ?_assertMatch({final, [v3, v4]},
				   run_test(fun(M) -> M(partial, v3), M(partial, v4), ok end,
							fun(W) -> w(10), cancellable_worker:check(W) end
						   )
	 ),
	 ?_assertMatch({partial, undefined},
				   run_test(fun(M)-> w(10), M(partial, v5) end,
							fun(W) -> cancellable_worker:check(W) end
						   )
	 ),
	 ?_assertMatch({final, v7},
				   run_test(fun(M) -> M(final, v7), ok end,
							fun(W) -> w(10), cancellable_worker:check(W) end
						   )
	 ),
	 ?_assertMatch({final, v8},
				   run_test(fun(M) -> M(final, v8), M(final, v9), ok end,
							fun(W) -> w(10), cancellable_worker:check(W) end
						   )
	 ),
	 ?_assertMatch({final, v10},
				   run_test(fun(M) -> M(final, v10), M(partial, v11), ok end,
							fun(W) -> w(10), cancellable_worker:check(W) end
						   )
	 ),
	 ?_assertMatch({final, [v12, v13]},
				   run_test(fun(M) -> M(partial, v12), M(final, v13), ok end,
							fun(W) -> w(10), cancellable_worker:check(W) end
						   )
	 ),
	 ?_assertMatch({{partial, [v12a]}, {partial, [v12a, v13a]}, {final, [v12a, v13a]}},
				   run_test(fun(M) -> M(partial, v12a), w(20), M(partial, v13a), w(20), ok end,
							fun(W) -> w(10), A=cancellable_worker:check(W),
									  w(30), B=cancellable_worker:check(W),
									  w(50), C=cancellable_worker:check(W),
									  {A, B, C}
							end
						   )
	 ),
	 ?_assertMatch({ok, [v14]},
				   run_test(fun(M) -> M(partial, v14), w(50), M(partial, v15), ok end,
							fun(W) -> w(10), cancellable_worker:cancel(W) end
						   )
	 ),
	 ?_assertMatch({ok, [v16, v17]},
				   run_test(fun(M) -> M(partial, v16), w(10), M(partial, v17), ok end,
							fun(W) -> w(30), cancellable_worker:cancel(W) end
						   )
	 ),
	 ?_assertMatch({final, [v18]},
				   run_test(fun(M) -> M(partial, v18), w(5), crash:crash(), w(5), M(partial, v19), ok end,
							fun(W) -> w(20), cancellable_worker:check(W) end
						   )
	 ),

	 ?_assertMatch({ok, undefined},
				   run_test(fun(_M)-> ok end,
							fun(W) -> cancellable_worker:yield(W) end
						   )
	 ),
	 ?_assertMatch({ok, [v6]},
				   run_test(fun(M) -> M(partial, v6) end,
							fun(W) -> w(10), cancellable_worker:yield(W) end
						   )
	 ),
	 ?_assertMatch({ok, [v19, v20]},
				   run_test(fun(M) -> M(partial, v19), M(partial, v20)  end,
							fun(W) -> w(10), cancellable_worker:yield(W) end
						   )
	 ),
	 ?_assertMatch({ok, [v21]},
				   run_test(fun(M) -> w(10), M(partial, v21) end,
							fun(W) -> cancellable_worker:yield(W) end
						   )
	 ),
	 ?_assertMatch({ok, [v22, v23]},
				   run_test(fun(M) -> M(partial, v22), M(final, v23)  end,
							fun(W) -> cancellable_worker:yield(W) end
						   )
	 ),
	 ?_assertMatch({ok, [v24]},
				   run_test(fun(M) -> M(partial, v24), w(5), crash:crash(), M(partial, v25), ok end,
							fun(W) -> cancellable_worker:yield(W) end
						   )
	 ),

	 	 ?_assert(true)
	].

w(N) ->
	receive after N -> ok end.

