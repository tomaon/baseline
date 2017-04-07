-module(baseline_ets).

-include("internal.hrl").

%% -- private --
-export([start_link/2]).
-export([tab/1]).
-export([delete/2,
         delete_all_objects/1,
         insert/2,
         insert_new/2,
         lookup/2,
         lookup_element/3,
         match/2,
         match_delete/2,
         member/2,
         select/2,
         select_count/2,
         select_delete/2,
         select_reverse/2,
         update_counter/3, update_counter/4,
         update_element/3
        ]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% -- internal --
-type(element_spec()  :: {non_neg_integer(), term()}|[{non_neg_integer(), term()}]).
-type(match_pattern() :: ets:match_pattern()).
-type(match_spec()    :: ets:match_spec()).
-type(tab()           :: ets:tab()).
-type(update_op()     :: {non_neg_integer(), integer()}|
                         {non_neg_integer(), integer(), integer(), integer()}).

%% == private ==

-spec start_link(atom(), [term()]) -> {ok, pid()}|{error, _}.
start_link(Name, Options) ->
    gen_server:start_link(?MODULE, [Name, Options], []).


-spec tab(pid()) -> tab().
tab(Pid) ->
    call(Pid, tab).


-spec delete(pid(), term()) -> true.
delete(Pid, Key) ->
    call(Pid, {delete, [Key]}).

-spec delete_all_objects(pid()) -> true.
delete_all_objects(Pid) ->
    call(Pid, {delete_all_objects, []}).

-spec insert(pid(), tuple()|[tuple()]) -> true.
insert(Pid, ObjectOrObjects) ->
    call(Pid, {insert, [ObjectOrObjects]}).

-spec insert_new(pid(), tuple()|[tuple()]) -> boolean().
insert_new(Pid, ObjectOrObjects) ->
    call(Pid, {insert_new, [ObjectOrObjects]}).

-spec lookup(pid(), term()) -> [tuple()].
lookup(Pid, Key) ->
    call(Pid, {lookup, [Key]}).

-spec lookup_element(pid(), term(), pos_integer()) -> term()|[term()].
lookup_element(Pid, Key, Pos) ->
    call(Pid, {lookup_element, [Key, Pos]}).

-spec match(pid(), match_pattern()) -> [term()].
match(Pid, Pattern) ->
    call(Pid, {match, [Pattern]}).

-spec match_delete(pid(), match_pattern()) -> true.
match_delete(Pid, Pattern) ->
    call(Pid, {match_delete, [Pattern]}).

-spec member(pid(), term()) -> boolean().
member(Pid, Key) ->
    call(Pid, {member, [Key]}).

-spec select(pid(), match_spec()) -> [term()].
select(Pid, MatchSpec) ->
    call(Pid, {select, [MatchSpec]}).

-spec select_count(pid(), match_spec()) -> non_neg_integer().
select_count(Pid, MatchSpec) ->
    call(Pid, {select_count, [MatchSpec]}).

-spec select_delete(pid(), match_spec()) -> non_neg_integer().
select_delete(Pid, MatchSpec) ->
    call(Pid, {select_delete, [MatchSpec]}).

-spec select_reverse(pid(), match_spec()) -> [term()].
select_reverse(Pid, MatchSpec) ->
    call(Pid, {select_reverse, [MatchSpec]}).

-spec update_counter(pid(), term(), update_op()) -> integer().
update_counter(Pid, Key, UpdateOp) ->
    call(Pid, {update_counter, [Key, UpdateOp]}).

-spec update_counter(pid(), term(), update_op()|[update_op()], tuple()) -> integer().
update_counter(Pid, Key, UpdateOp, Default) ->
    call(Pid, {update_counter, [Key, UpdateOp, Default]}).

-spec update_element(pid(), term(), element_spec()) -> boolean().
update_element(Pid, Key, ElementSpec) ->
    call(Pid, {update_element, [Key, ElementSpec]}).

%% -- behaviour: gen_server --

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({Function, Args}, _From, Tab) ->
    try apply(ets, Function, [Tab|Args]) of
        Term ->
            {reply, {ok, Term}, Tab}
    catch
        error:Reason ->
            {reply, {error, Reason}, Tab}
    end;
handle_call(tab, _From, Tab) -> % sys:get_state/1 ?
    {reply, {ok, Tab}, Tab}.

handle_cast(_Request, State) ->
    {stop, enosys, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State}.

%% == internal ==

cleanup(Tab)
  when Tab =/= undefined ->
    true = ets:delete(Tab),
    cleanup(undefined);
cleanup(undefined) ->
    baseline:flush().

setup(Args) ->
    try apply(ets, new, Args) of
        Tab ->
            false = process_flag(trap_exit, true),
            {ok, Tab}
    catch
        error:Reason ->
            {stop, Reason}
    end.


call(Pid, Request) ->
    case gen_server:call(Pid, Request, 10000) of
        {ok, Term} ->
            Term;
        {error, Reason} ->
            error(Reason)
    end.
