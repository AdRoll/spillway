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
-module(spillway_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, enter/3, leave/2, cur/1, state/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TID, spillway).

-record(counter, {name, value = 0 :: non_neg_integer()}).

%%%===================================================================
%%% External functions
%%%===================================================================

%% Attempt to increment the named counter, respecting the given limit.  If the counter was
%% successfully incremented, return {true, NewValue}.  Otherwise, return false.
-spec enter(term(), non_neg_integer(), non_neg_integer()) ->
               false | {true, non_neg_integer()}.
enter(Name, Size, Limit) when Size > 0 ->
    case cur(Name) of
        Value when Value + Size > Limit ->
            false;
        _ ->
            %% note: update_counter accepts a list of operations.  we need to know whether
            %% we were the process to successfully increment a limit-reaching value, so we
            %% use an initial non-incrementing operation to read the existing value.  if
            %% the result is [X, X+Size], we successfully incremented the counter.  if we
            %% failed, the result will be [X, X].
            [OldValue, NewValue] =
                ets:update_counter(?TID,
                                   Name,
                                   [{#counter.value, 0}, {#counter.value, Size, Limit, Limit}],
                                   #counter{name = Name}),
            Expected = OldValue + Size,
            case NewValue of
                OldValue ->
                    %% We did not increment
                    false;
                Expected ->
                    %% We incremented
                    {true, Expected};
                Limit ->
                    %% We incremented over the limit so limit is set
                    {true, Limit}
            end
    end.

%% Attempt to decrement the named counter, with a lower limit of 0.  Return the new value.
%% The counter must already exist.
-spec leave(term(), non_neg_integer()) -> non_neg_integer().
leave(Name, Size) ->
    ets:update_counter(?TID, Name, {#counter.value, -Size, 0, 0}).

%% Return the current counter value.
-spec cur(term()) -> non_neg_integer().
cur(Name) ->
    try
        ets:lookup_element(?TID, Name, #counter.value)
    catch
        error:badarg ->
            0
    end.

%% For debug purposes, return the state of all counters
-spec state() -> [term()].
state() ->
    ets:tab2list(?TID).

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    create_ets(),
    {ok, stateless}.

handle_call(_Request, _From, State) ->
    Reply = {error, not_implemented},
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% note: we own the ets, and it will be automatically deleted when we terminate.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%% Internal Functions
create_ets() ->
    ets:new(?TID,
            [set,
             named_table,
             public,
             {keypos, #counter.name},
             {read_concurrency, true},
             {write_concurrency, true}]).
