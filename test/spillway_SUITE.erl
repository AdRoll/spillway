%%-------------------------------------------------------------------
%% The MIT License (MIT)
%% Copyright (c) 2018 AdRoll, Inc. Miriam Pena and Mike Watters
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%
%%-------------------------------------------------------------------
-module(spillway_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, suite/0, init_per_testcase/2,
         end_per_testcase/2]).
-export([complex/1, simple/1]).

-define(TABLE, test_counter).

%%%=============================================================================
%%% common_test callbacks
%%%=============================================================================

all() ->
    [simple, complex].

suite() ->
    [{timetrap, {seconds, 15}}].

init_per_suite(Conf) ->
    Conf.

end_per_suite(_Conf) ->
    ok.

init_per_testcase(_Module, Conf) ->
    ok = application:start(spillway),
    Conf.

end_per_testcase(_Module, _Conf) ->
    ok = application:stop(spillway),
    ok.

%%%=============================================================================
%%% Tests
%%%=============================================================================

simple(_) ->
    ?assertEqual(false, spillway:enter(?TABLE, 0)),
    ?assertEqual({true, 1}, spillway:enter(?TABLE, 2)),
    ?assertEqual({true, 2}, spillway:enter(?TABLE, 2)),
    ?assertEqual(false, spillway:enter(?TABLE, 2)),
    ?assertEqual(1, spillway:leave(?TABLE)),
    ?assertEqual(0, spillway:leave(?TABLE)),
    ?assertEqual(0, spillway:leave(?TABLE)),
    ok.

spawn_proc(ProcN, Limit, SignalGo, SignalStop, Parent) ->
    spawn_monitor(fun () ->
                          monitor(process, SignalGo),
                          monitor(process, SignalStop),
                          receive
                              {'DOWN', _, process, SignalGo, _} ->
                                  case spillway:enter(?TABLE, ProcN, Limit) of
                                      {true, N} ->
                                          send_parent(Parent, {entered, self(), N}),
                                          receive
                                              {'DOWN', _, process, SignalStop, _} ->
                                                  spillway:leave(?TABLE, ProcN)
                                          end;
                                      false ->
                                          send_parent(Parent, {not_entered, self()}),
                                          ok
                                  end
                          end
                  end).

complex(_) ->
    NProcs = 2000,
    Limit = 140000,
    Parent = self(),
    SignalGo = signal(),
    SignalStop = signal(),
    Processes =
        [element(1, spawn_proc(ProcN, Limit, SignalGo, SignalStop, Parent))
         || ProcN <- lists:seq(1, NProcs)],
    SignalGo ! go,

    %% Collect enter msg
    N = collect_values(Processes, 0),
    ?assert(N =< Limit),
    Value = spillway:cur(?TABLE),
    ?assert(Value =< Limit),

    %% wait for all the workers to leave and finish
    SignalStop ! go,
    wait_for_down(Processes),
    ?assertMatch(0, spillway:cur(?TABLE)),
    ok.

send_parent(Parent, Msg) ->
    Parent ! Msg.

collect_values([], Max) ->
    Max;
collect_values(ProcessesSignal, Cur) ->
    receive
        {not_entered, Pid} ->
            collect_values(ProcessesSignal -- [Pid], Cur);
        {entered, Pid, M} when M > Cur ->
            collect_values(ProcessesSignal -- [Pid], M);
        {entered, Pid, _} ->
            collect_values(ProcessesSignal -- [Pid], Cur)
    end.

wait_for_down([]) ->
    ok;
wait_for_down(ProcessesExited) ->
    receive
        {'DOWN', _, process, Pid, _} ->
            wait_for_down(ProcessesExited -- [Pid])
    end.

signal() ->
    spawn(fun () ->
                  receive
                      go ->
                          ok
                  end
          end).
