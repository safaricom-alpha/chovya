%%%-------------------------------------------------------------------
%%% @author's gihub username: @cmush
%%% @author's email address: collinsmucheru
%%% @copyright (C) 2019, @SafaricomAlpha
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2019 21:32
%%%-------------------------------------------------------------------
-module(chovya_utils).
-author("collinsmucheru@gmail.com").

%% API
-export([
    generate_filter_with_and/2,
    generate_filter_with_comma/2,
    date_time/0,
    date_time/1
]).

generate_filter_with_comma({FieldAtom, Value}, {FieldElements, ValueElements}) ->
    FieldName = erlang:atom_to_binary(FieldAtom, latin1),
    case FieldElements of
        <<"">> ->
            Fields = <<FieldName/bitstring, <<" = ?">>/bitstring>>,
            {Fields, [Value]};
        _ ->
            Fields = <<FieldElements/bitstring, <<", ">>/bitstring, FieldName/bitstring, <<" = ?">>/bitstring>>,
            Values = ValueElements ++ [Value],
            {Fields, Values}
    end.

generate_filter_with_and({FieldAtom, Value}, {FieldElements, ValueElements}) ->
    FieldName = erlang:atom_to_binary(FieldAtom, latin1),
    case FieldElements of
        <<"">> ->
            Fields = <<FieldName/bitstring, <<" = ?">>/bitstring>>,
            {Fields, [Value]};
        _ ->
            Fields = <<FieldElements/bitstring, <<" AND ">>/bitstring, FieldName/bitstring, <<" = ?">>/bitstring>>,
            Values = ValueElements ++ [Value],
            {Fields, Values}
    end.

date_time() ->
    date_time(calendar:local_time()).
date_time({{Y, Mo, D}, {H, M, S}}) ->
    TimeList = [lists:flatten(integer_to_list(Y)) ++
                lists:flatten("-") ++
                lists:flatten(integer_to_list(Mo)) ++
                lists:flatten("-") ++
                lists:flatten(integer_to_list(D)) ++
                lists:flatten(" ") ++
                lists:flatten(integer_to_list(H)) ++
                lists:flatten(":") ++
                lists:flatten(integer_to_list(M)) ++
                lists:flatten(":") ++
                lists:flatten(integer_to_list(S))],
    list_to_binary(TimeList).