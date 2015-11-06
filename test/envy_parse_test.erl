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
%% @copyright 2013 Mark Anderson

-module(envy_parse_test).

-include_lib("eunit/include/eunit.hrl").

parse_to_ip_simple_test_() ->
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [{"parses ipv4 literal with ipv4 enabled",
       ?_test(?assertEqual({127,0,0,1}, envy_parse:to_ip("127.0.0.1", [ipv4] )))
      },
      {"parses ipv6 literal",
       ?_test(?assertEqual({0,0,0,0,0,0,0,1}, envy_parse:to_ip("::1", [ipv6] )))
      },
      {"fails to parse ipv4 when ipv6 is wanted",
       ?_test(?assertError({bad_host, "127.0.0.1"}, envy_parse:to_ip("127.0.0.1", [ipv6])))
      },
      {"fails to parse ipv6 when ipv4 is wanted",
       ?_test(?assertError({bad_host, "::1"}, envy_parse:to_ip("::1", [ipv4])))
      },
      {"parses ipv4 literal with ipv4 enabled second",
       ?_test(?assertEqual({127,0,0,1}, envy_parse:to_ip("127.0.0.1", [ipv6, ipv4] )))
      },
      {"parses ipv6 literal with ipv6 enabled second",
       ?_test(?assertEqual({0,0,0,0,0,0,0,1}, envy_parse:to_ip("::1", [ipv4, ipv6] )))
      },
      {"parses hostname that resolves to ipv4",
       ?_test(?assertMatch({_,_,_,_}, envy_parse:to_ip("localhost", [ipv4, ipv6] )))
      }
     ]}.

host_to_ip_default_value_test() ->
    ?assertEqual({127,0,0,1}, envy_parse:host_to_ip(bad_section, bad_key, "127.0.0.1")).

host_to_ip_simple_test_() ->
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [{"parses ipv4 literal with ipv4 enabled",
       fun () ->
               setup_host(app_key, host, "127.0.0.1", [ipv4]),
               ?assertEqual({127,0,0,1}, envy_parse:host_to_ip(app_key, host))
       end
      },
      {"parses ipv6 literal",
       fun () ->
               setup_host(app_key, host, "::1", [ipv6]),
               ?assertEqual({0,0,0,0,0,0,0,1}, envy_parse:host_to_ip(app_key, host))
       end
      }
     ]}.

setup_host(App, Key, Value, Preferences) ->
    application:set_env(App, Key, Value),
    application:set_env(App, ip_mode, Preferences).




