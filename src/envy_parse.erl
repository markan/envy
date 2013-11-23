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

%% @doc Simple error checking functions for application:get_env
-module(envy_parse).

-export([to_ip/2,
         host_to_ip/2,
         host_to_ip/3
        ]).

-type envy_ip_preference() :: 'ipv4' | 'ipv6'.

-spec to_ip(string(), [envy_ip_preference()] ) -> inet:ip_address().
to_ip(Host, []) ->
    erlang:error({no_prefered_parse_succeeded, Host});
to_ip(Host, [ipv4 | Tail]) ->
    maybe_parse(inet:getaddr(Host, inet), Host, Tail);
to_ip(Host, [ipv6 | Tail]) ->
    maybe_parse(inet:getaddr(Host, inet6), Host, Tail).

maybe_parse({error, nxdomain}, Host, []) ->
    erlang:error({bad_host, Host});
maybe_parse({error, nxdomain}, Host, Preferences) ->
    to_ip(Host, Preferences);
maybe_parse({error, Error}, Host, _Preferences) ->
    erlang:error({Error, Host});
maybe_parse({ok, IP}, _Host, _Preferences) ->
    IP.

%% These functions look for a string typed key under Section, Item, and parses it down to
%% an ipv4/ipv6 address based on a preferences string kept in Section, 'ip_mode'
host_to_ip(Section, Item) ->
    Host = envy:get(Section, Item, string),
    Preferences = envy:get(Section, ip_mode, [ipv4], list),
    to_ip(Host, Preferences).

host_to_ip(Section, Item, Default) ->
    Host = envy:get(Section, Item, string, Default),
    Preferences = envy:get(Section, ip_mode, [ipv4], list),
    to_ip(Host, Preferences).
