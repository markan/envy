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
         get/4
        ]).

-include_lib("eunit/include/eunit.hrl").

-type envy_type_validator() :: fun( (any()) -> boolean()).
-type envy_type_constraint() :: envy_type_validator | atom().

-spec fun_ex('any' | 'atom' | 'boolean' | 'bool' | 'float' | 'integer' | 'int' | 'positive_integer' | 'number' | 'string' | envy_type_validator()) -> fun().
fun_ex(any) -> fun(_) -> true end;
fun_ex(atom) -> fun is_atom/1;
fun_ex(binary) -> fun is_binary/1;
fun_ex(bool) -> fun is_boolean/1;
fun_ex(boolean) -> fun is_boolean/1;
fun_ex(float) -> fun is_float/1;
fun_ex(int) -> fun is_integer/1;
fun_ex(integer) -> fun is_integer/1;
fun_ex(positive_integer) -> fun(Val) -> is_integer(Val) andalso Val > 0 end;
fun_ex(number) -> fun is_number/1;
fun_ex(string) -> fun is_list/1;
fun_ex(F) when is_function(F) -> F.

-spec get(atom(), atom(), envy_type_constraint() ) -> any().
get(Section, Item, TypeCheck) ->
    TypeCheckF = fun_ex(TypeCheck),
    case application:get_env(Section, Item) of
        {ok, Value} ->
            case TypeCheckF(Value) of
                true -> Value;
                Error ->
                    error_logger:error_msg("Bad typecheck for config item for '~p' '~p' (~p(~p) -> ~p)~n",
                                           [Section, Item, TypeCheck, Value, Error]),
                    error(config_bad_type)
            end;
        undefined ->
            error_logger:error_msg("Missing config item for '~p' '~p'~n", [Section, Item]),
            error(config_missing_item)
    end.

-spec get(atom(), atom(), any(), envy_type_constraint() ) -> any().
get(Section, Item, Default, TypeCheck) ->
    TypeCheckF = fun_ex(TypeCheck),
    case application:get_env(Section, Item) of
        {ok, Value} ->
            case TypeCheckF(Value) of
                true -> Value;
                Error ->
                    error_logger:error_msg("Bad typecheck for config item for '~p' '~p' (~p(~p) -> ~p)~n",
                                           [Section, Item, TypeCheck, Value, Error]),
                    error(config_bad_type)
            end;
        undefined ->
            Default
    end.
