%%%-------------------------------------------------------------------
%%% @author's gihub username: @cmush
%%% @author's email address: collinsmucheru
%%% @copyright (C) 2019, @SafaricomAlpha
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2019 21:30
%%%-------------------------------------------------------------------
-module(chovya).
-author("collinsmucheru@gmail.com").

-behaviour(gen_server).

-include_lib("config_parser/include/config_parser.hrl").

%% test api
-export([
    start/1,
    stop/0
]).

%% api
-export([
    start_link/1,
    query/1,
    query/2,
    insert/2,
    update/3,
    select/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% test api functions
%%====================================================================
-spec start(proplists:proplist()) -> {ok, pid()} | {error, any()}.
start(ConnectionArgs) ->
    MySQLPoolOpts = [{workers, 1},
                     {worker, {chovya, ConnectionArgs}},
                     {pool_sup_shutdown, infinity}],
    application:ensure_all_started(worker_pool),
    wpool:start_sup_pool(mysql_pool, MySQLPoolOpts).

-spec stop() -> any().
stop() ->
    wpool:stop_sup_pool(mysql_pool).

%%====================================================================
%% api functions
%%====================================================================
-spec start_link(proplists:prolist()) -> {ok, pid()} | {error, any()}.
start_link(ConnectionArgs) ->
    MySQLPoolOpts = [{workers, proplists:get_value(pool_size, ConnectionArgs, 1)},
                     {worker, {chovya, ConnectionArgs}},
                     {pool_sup_shutdown, infinity}],
    lager:info("chovya PoolOpts = ~p~n", [MySQLPoolOpts]),
    application:ensure_all_started(worker_pool),
    wpool:start_pool(mysql_pool, MySQLPoolOpts).

-spec query(iodata()) -> any().
query(Query) ->
    call_worker({query, [Query]}).

-spec query(iodata(), list()) -> any().
query(Query, Args) ->
    call_worker({query, [Query, Args]}).

insert(Table, FieldsAndValues) ->
    TableName = atom_to_binary(Table, latin1),

    Fun = fun({FieldAtom, _}, {FieldElements, ValueElements}) ->
        FieldName = erlang:atom_to_binary(FieldAtom, latin1),
        case FieldElements of
            <<"">> ->
                Fields = <<FieldElements/bitstring, FieldName/bitstring>>,
                Values = <<ValueElements/bitstring, <<"?">>/bitstring>>,
                {Fields, Values};
            _ ->
                Fields = <<FieldElements/bitstring, <<",">>/bitstring, FieldName/bitstring>>,
                Values = <<ValueElements/bitstring, <<",">>/bitstring, <<"?">>/bitstring>>,
                {Fields, Values}
        end
          end,

    {Fields, Values} = lists:foldl(Fun, {<<"">>, <<"">>}, FieldsAndValues),

    Query =
    <<
        <<"Insert Into ">>/bitstring, TableName/bitstring, <<" (">>/bitstring, Fields/bitstring, <<") values ( ">>/bitstring, Values/bitstring, <<")">>/bitstring
    >>,
    lager:info("insert Query: ~p", [Query]),

    query(Query, [N || {_, N} <- FieldsAndValues]).

update(Table, NewFieldsAndValues, FilterFieldsAndValues) ->
    TableName = atom_to_binary(Table, latin1),

    FunComma = fun chovya_utils:generate_filter_with_comma/2,
    {NewFields, NewValues} = lists:foldl(FunComma, {<<"">>, []}, NewFieldsAndValues),

    FunAnd = fun chovya_utils:generate_filter_with_and/2,
    {FilterFields, FilterValues} = lists:foldl(FunAnd, {<<"">>, []}, FilterFieldsAndValues),

    Filter = case FilterFields of
                 <<"">> ->
                     <<"">>;
                 _ ->
                     <<
                         <<" WHERE ">>/bitstring, FilterFields/bitstring
                     >>
             end,

    Query =
    <<
        <<"UPDATE ">>/bitstring, TableName/bitstring,
        <<" SET ">>/bitstring, NewFields/bitstring,
        Filter/bitstring
    >>,
    lager:info("update Query: ~p", [Query]),

    query(Query, NewValues ++ FilterValues).

select(Table, SelectFields, FilterFieldsAndValues) ->
    TableName = atom_to_binary(Table, latin1),

    Fun = fun(Field, FieldsBitString) ->
        FieldName = erlang:atom_to_binary(Field, latin1),
        case FieldsBitString of
            <<"">> ->
                FieldName;
            _ ->
                <<FieldsBitString/bitstring, <<", ">>/bitstring, FieldName/bitstring>>
        end
          end,

    SelectBitString = case lists:foldl(Fun, <<"">>, SelectFields) of
                          <<"">> ->
                              <<"*">>;
                          BitString ->
                              BitString
                      end,

    Fun2 = fun chovya_utils:generate_filter_with_and/2,
    {FilterFields, FilterValues} = lists:foldl(Fun2, {<<"">>, []}, FilterFieldsAndValues),

    Filter = case FilterFields of
                 <<"">> ->
                     <<"">>;
                 _ ->
                     <<
                         <<" WHERE ">>/bitstring, FilterFields/bitstring
                     >>
             end,

    Query =
    <<
        <<"SELECT ">>/bitstring, SelectBitString/bitstring,
        <<" FROM ">>/bitstring, TableName/bitstring,
        Filter/bitstring
    >>,
    lager:info("select Query: ~p", [Query]),

    query(Query, FilterValues).

%%====================================================================
%% gen_server callbacks
%%====================================================================
-spec init(list()) -> {ok, map()}.
init(ConnectionArgs) ->
    process_flag(trap_exit, true),
    lager:info("chovya starting"),
    lager:debug("chovya ConnectionArgs = ~p", [ConnectionArgs]),
    {ok, Pid} = mysql:start_link(ConnectionArgs),
    {ok, #{pid => Pid}}.

-spec handle_call(any(), {pid(), any()}, map()) -> {reply, any(), map()}.
handle_call({query, Args}, _From, #{pid := Pid} = State) ->
    {reply, mysql:query(Pid, Args), State};
handle_call(_Event, _From, State) ->
    {reply, {error, badarg}, State}.

-spec handle_cast(any(), map()) -> {noreply, map()}.
handle_cast(Request, State) ->
    lager:warn("unexpected cast: ~p", [Request]),
    {noreply, State}.

-spec code_change(any(), map(), any()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec handle_info(any(), map()) -> {stop, any(), map()} | {noreply, map()}.
handle_info({'EXIT', _Pid, _Reason} = Reason, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    lager:info("unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(any(), map()) -> any().
terminate(_Reason, #{pid := Pid}) ->
    gen_server:stop(Pid).

%%% Internal functions
call_worker(Request) ->
    wpool:call(mysql_pool, Request, best_worker, transaction_timeout()).

transaction_timeout() ->
    5000.
