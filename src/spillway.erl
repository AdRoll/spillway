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
-module(spillway).

-export([enter/2,
         enter/3,
         leave/1,
         leave/2,
         cur/1,
         state/0]).

%%%===================================================================
%%% External functions
%%%===================================================================

%% Attempt to increment the named counter, respecting the given limit.  If the counter was
%% successfully incremented, return {true, NewValue}.  Otherwise, return false.  The
%% counter must already exist.
-spec enter(term(), non_neg_integer()) -> false | {true, non_neg_integer()}.
enter(Name, Limit) ->
    enter(Name, 1, Limit).

-spec enter(term(), non_neg_integer(), non_neg_integer()) -> false | {true, non_neg_integer()}.
enter(Name, Size, Limit) when Size > 0 ->
    spillway_srv:enter(Name, Size, Limit).

%% Attempt to decrement the named counter, with a lower limit of 0.  Return the new value.
%% The counter must already exist.
-spec leave(term()) -> non_neg_integer().
leave(Name) ->
    leave(Name, 1).

-spec leave(term(), non_neg_integer()) -> non_neg_integer().
leave(Name, Size) ->
    spillway_srv:leave(Name, Size).


%% Return the current counter value.  The counter must already exist.
-spec cur(term()) -> non_neg_integer().
cur(Name) ->
    spillway_srv:cur(Name).

%% For debug purposes. Returns the state of all counters.
state() ->
    spillway_srv:state().
