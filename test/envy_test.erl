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
%% @copyright 2012 Mark Anderson

-module(envy_test).

-include_lib("eunit/include/eunit.hrl").



get_simple_test_() ->
    {foreach,
     fun() ->

             application:set_env(testing, ival, 1),
             application:set_env(testing, aval, an_atom),
             application:set_env(testing, bval, true),
             application:set_env(testing, sval, "foo"),
             application:set_env(testing, lval, [foo, bar, baz]),
             application:set_env(testing, binaryval, <<"bar">>),
             application:set_env(testing, invalid, {junk}),
             application:set_env(testing, nival, -1),
             application:set_env(testing, zival, 0),
             ok
     end,
     fun(_) ->
             ok
     end,
     [{"fails for missing",
       ?_test(?assertError(config_missing_item, envy:get(testing, no_such_value, any)))
      },
      {"integer test passes",
       ?_test(?assertEqual(1, envy:get(testing, ival, integer)))
      },
      {"integer type fails",
       ?_test(?assertError(config_bad_type, envy:get(testing, sval, integer)))
      },
      {"positive_integer test passes",
       ?_test(?assertEqual(1, envy:get(testing, ival, positive_integer)))
       },
      {"pos_integer alias test passes",
       ?_test(?assertEqual(1, envy:get(testing, ival, pos_integer)))
        },
      {"positive_integer type fails on negative value",
       ?_test(?assertError(config_bad_type, envy:get(testing, nival, positive_integer)))
       },
      {"positive_integer type fails on zero value",
       ?_test(?assertError(config_bad_type, envy:get(testing, zival, positive_integer)))
       },
      {"bool test passes",
       ?_test(?assertEqual(true, envy:get(testing, bval, boolean)))
      },
      {"bool type fails passes",
       ?_test(?assertError(config_bad_type, envy:get(testing, sval, boolean)))
      },
      {"list type fails on non-list",
       ?_test(?assertError(config_bad_type, envy:get(testing, aval, list)))
       },
      {"list test passes",
       ?_test(?assertEqual([foo, bar, baz], envy:get(testing, lval, list)))
       },
      {"non_neg_integer test passes",
       ?_test(?assertEqual(0, envy:get(testing, zival, non_neg_integer)))
       },
      {"non_neg_integer type fails on negative value",
       ?_test(?assertError(config_bad_type, envy:get(testing, nival, non_neg_integer)))
       },
      {"composite test passes if one matches",
       ?_test(?assertEqual(0, envy:get(testing, zival, [pos_integer, non_neg_integer])))
       },
      {"composite test fails if all fail",
       ?_test(?assertError(config_bad_type, envy:get(testing, nival, [pos_integer, non_neg_integer])))
       }
     ]}.


matrix_test_() ->
    M =  mk_test_matrix(),
    ?debugVal(M),
    Types = lists:usort(lists:flatten([ TypeList || {_, TypeList} <- M])),
    ?debugVal(Types),
    lists:flatten([mk_test(TypeName, Item, ValidTypes) || TypeName <-Types,
                                                          {Item, ValidTypes} <- M]).

mk_test(TypeName, Item, ValidTypes) ->
    case lists:member(TypeName, ValidTypes) of
        true ->
            mk_pass_test(TypeName, Item);
        false ->
            mk_fail_test(TypeName, Item)
    end.

mk_pass_test(TypeName, Item) ->
    Desc = iolist_to_binary(io_lib:format("Accepts ~w when type is ~s", [Item, TypeName])),
    Fun = fun() ->
                  application:set_env(testing, testval, Item),
                  ?assertEqual(Item, envy:get(testing, testval, TypeName))
          end,
    {Desc, Fun}.

mk_fail_test(TypeName, Item) ->
    Desc = iolist_to_binary(io_lib:format("Rejects ~w when type is ~s", [Item, TypeName])),
    Fun = fun() ->
                  application:set_env(testing, testval, Item),
                  ?assertError(config_bad_type, envy:get(testing, testval, TypeName))
          end,
    {Desc, Fun}.

mk_test_matrix() ->
    [{an_atom, [atom]},
     {'2_be_another_atom', [atom]},
     {0, [int, integer, number]},
     {1, [int, integer, number]},
     {2.0, [float, number]},
     {<<"">>, [binary]},
     {<<"another_binary">>, [binary]},
     {"", [string]},
     {"another string", [string]}].
