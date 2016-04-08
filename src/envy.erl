%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%%
%% Copyright Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2012 Opscode Inc

%% @doc Simple error checking functions for application:get_env
-module(envy).

-export([get/3,
         get/4,
         proplist_get/3,
         proplist_get/4,
         one_of/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-type envy_type_validator() :: fun( (any()) -> boolean()).
-type envy_type_constraint() :: envy_type_validator() |
                                'any' |
                                'atom' |
                                'binary' |
                                'bool' |
                                'boolean' |
                                'float' |
                                'int' |
                                'integer' |
                                'list' |
                                'non_neg_integer' |
                                'number' |
                                'pos_integer' |
                                'positive_integer' |
                                'string'.
-type envy_type_constraints() :: envy_type_constraint() |
                                 nonempty_list(envy_type_constraint()).

-spec fun_ex(envy_type_constraints()) -> envy_type_validator().
fun_ex(any) -> fun(_) -> true end;
fun_ex(atom) -> fun is_atom/1;
fun_ex(binary) -> fun is_binary/1;
fun_ex(bool) -> fun is_boolean/1;
fun_ex(boolean) -> fun is_boolean/1;
fun_ex(float) -> fun is_float/1;
fun_ex(int) -> fun is_integer/1;
fun_ex(integer) -> fun is_integer/1;
fun_ex(pos_integer) -> fun_ex(positive_integer);
fun_ex(positive_integer) -> fun(Val) -> is_integer(Val) andalso Val > 0 end;
fun_ex(non_neg_integer) -> fun(Val) -> is_integer(Val) andalso Val >= 0 end;
fun_ex(number) -> fun is_number/1;
fun_ex(List) when is_list(List) ->
    fun(Val) ->
            lists:any(fun(FunEx) -> FunEx(Val) end,
                      [fun_ex(ListEntry) || ListEntry <- List])
    end;
fun_ex(list) -> fun_ex(string);
fun_ex(string) -> fun is_list/1;
fun_ex(F) when is_function(F) -> F.

-spec one_of(list()) -> function().
one_of(List) when is_list(List) ->
    fun(Item) ->
            lists:member(Item, List)
    end.

-spec get(atom(), atom(), envy_type_constraints() ) -> any().
get(Section, Item, TypeCheck) ->
    get_validate(application:get_env(Section, Item), Section, Item, TypeCheck).

-spec get(atom(), atom(), any(), envy_type_constraints() ) -> any().
get(Section, Item, Default, TypeCheck) ->
    case application:get_env(Section, Item) of
        {ok, Value} ->
            get_validate({ok, Value}, Section, Item, TypeCheck);
        undefined ->
            Default
    end.

get_validate(undefined, Section, Item, _TypeCheck) ->
    error_logger:error_msg("Missing config item for '~p' '~p'~n", [Section, Item]),
    error(config_missing_item);
get_validate({ok, Value}, Section, Item, TypeCheck) ->
    TypeCheckF = fun_ex(TypeCheck),
    case TypeCheckF(Value) of
        true -> Value;
        Error ->
            error_logger:error_msg("Bad typecheck for config item for '~p' '~p' (~p(~p) -> ~p)~n",
                                   [Section, Item, TypeCheck, Value, Error]),
            error(config_bad_type)
    end.

-spec proplist_get(atom(), envy_type_constraints(), list()) -> any().
proplist_get(Key, TypeCheck, PropList) ->
    proplist_validate(proplists:get_value(Key, PropList), Key, TypeCheck).

-spec proplist_get(atom(), envy_type_constraints(), list(), any()) -> any().
proplist_get(Key, TypeCheck, PropList, Default) ->
    proplist_validate(proplists:get_value(Key, PropList, Default), Key, TypeCheck).

proplist_validate(undefined, Key, _) ->
    error_logger:error_msg("Missing config item for '~p'~n", [Key]),
    error(config_missing_item);
proplist_validate(Value, Key, TypeCheck) ->
    TypeCheckF = fun_ex(TypeCheck),
    case TypeCheckF(Value) of
        true -> Value;
        Error ->
            error_logger:error_msg("Bad typecheck for config item for '~p' (~p(~p) -> ~p)~n",
                                   [Key, TypeCheck, Value, Error]),
            error(config_bad_type)
    end.
