%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% Copyright (C) 2014 Petr Gotthard <petr.gotthard@centrum.cz>
%%

-module(rabbit_auth_backend_ip_range).

-behaviour(rabbit_authz_backend).

-include_lib("rabbit_common/include/rabbit.hrl").

-export([description/0]).
-export([user_login_authorization/1, user_login_authorization/2,
         check_vhost_access/3, check_resource_access/3, check_topic_access/4,
         state_can_expire/0]).

description() ->
    [{name, <<"IP_Range">>},
     {description, <<"IP based client authorization">>}].

user_login_authorization(_Username) ->
    {ok, none}.

user_login_authorization(_Username, _AuthProps) ->
    {ok, none}.

check_vhost_access(#auth_user{tags = Tags}, _VHostPath, AuthzData) ->
    Address = extract_address(AuthzData),

    % filter out applicable masks
    case lists:filtermap(
            fun({Tag, Masks}) ->
                case lists:member(Tag, Tags) of
                    true -> {true, Masks};
                    false -> false
                end
            end, env(tag_masks)) of
        []   -> check_masks(Address, env(default_masks));
        Else -> check_masks(Address, lists:flatten(Else))
    end.

check_masks(undefined, _Masks) -> true; % allow internal access
check_masks(Address, Masks) ->
    R = lists:foldl(
        fun(StrMask, false) ->
            {Mask, Bits} = compile_addrmask(StrMask),
            Addr = address_to_binary(Address, Bits),
            if
                Addr == Mask -> true;
                true         -> false
            end;
           (_, true) -> true
        end, false, Masks),
    if
        R == false ->
            rabbit_log:warning("Address ~s not matching any of [ ~s]~n",
                [inet_parse:ntoa(Address), format_masks(Masks)]),
            false;
        true ->
            rabbit_log:debug("Address ~s matching [ ~s]~n",
                [inet_parse:ntoa(Address), format_masks(Masks)]),
            true
    end.

check_resource_access(#auth_user{}, #resource{}, _Permission, _Context) -> true.

check_topic_access(#auth_user{}, #resource{}, _Permission, _Context) -> true.

state_can_expire() -> false.

%%--------------------------------------------------------------------

env(F) ->
    {ok, V} = application:get_env(rabbitmq_auth_backend_ip_range, F),
    V.

extract_address(undefined) ->
    undefined;
extract_address(#{peeraddr := Address}) ->
    Address.

format_masks(Masks) ->
    lists:flatten(lists:foldr(fun(Mask, Acc) ->
        [io_lib:format("~s ", [Mask]) | Acc] end, [], Masks)).

compile_addrmask(AddrMask) ->
    {ABin, ABits} =
        case string:tokens(binary_to_list(AddrMask), "/\\") of
            [Addr] ->
                compile_address(Addr);
            [Addr, Bits] ->
                {Addr2, _} = compile_address(Addr),
                {Addr2, list_to_integer(Bits)}
        end,
    {address_to_binary(ABin, ABits), ABits}.

compile_address(Addr) ->
    case inet:parse_address(Addr) of
        {ok, Address4} when size(Address4) == 4 -> {Address4, 32};
        {ok, Address6} when size(Address6) == 8 -> {Address6, 128};
        {error, _} -> throw({error, einval})
    end.

address_to_binary({B1, B2, B3, B4}, 32) ->
    <<B1:8, B2:8, B3:8, B4:8>>;

address_to_binary({B1, B2, B3, B4}, Bits) when Bits < 32 ->
    <<Subset:Bits/bitstring, _Others/bitstring>> = <<B1:8, B2:8, B3:8, B4:8>>,
    Subset;

address_to_binary({B1, B2, B3, B4}, Bits) when Bits > 32 ->
    % matching the IPv4-mapped IPv6 address
    <<Subset:Bits/bitstring, _Others/bitstring>> = <<0:80, 16#FFFF:16, B1:8, B2:8, B3:8, B4:8>>,
    Subset;

address_to_binary({W1, W2, W3, W4, W5, W6, W7, W8}, Bits) ->
    <<Subset:Bits/bitstring, _Others/bitstring>> = <<W1:16, W2:16, W3:16, W4:16, W5:16, W6:16, W7:16, W8:16>>,
    Subset;

address_to_binary(_, 0) -> <<0>>.

%% end of file
