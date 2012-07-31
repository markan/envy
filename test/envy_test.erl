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

-module(envy_test).

-include_lib("eunit/include/eunit.hrl").



get_simple_test_() ->
    {foreach,
     fun() ->
             application:set_env(testing, ival, 1),
             application:set_env(testing, bval, true),
             application:set_env(testing, sval, "foo"),
             application:set_env(testing, binaryval, <<"bar">>),
             application:set_env(testing, invalid, {junk}),
             ok
     end,
     fun(_) ->
             ok
     end,
     [{"fails for missing",
       fun() ->
               ?assertError(config_missing_item, envy:get(testing, no_such_value, any))
       end
      },
      {"integer test passes",
       fun() ->
               ?assertEqual(1, envy:get(testing, ival, integer))
       end
      },
      {"integer type fails",
       fun() ->
               ?assertError(config_bad_type, envy:get(testing, sval, integer))
       end
      },
      {"bool test passes",
       fun() ->
               ?assertEqual(true, envy:get(testing, bval, boolean))
       end
      },
      {"bool type fails passes",
       fun() ->
               ?assertError(config_bad_type, envy:get(testing, sval, boolean))
       end
      }
     ]}.
