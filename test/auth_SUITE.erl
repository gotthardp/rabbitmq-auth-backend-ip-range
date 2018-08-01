%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(auth_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

all() -> [
          guest_not_allowed,
          private_user_not_allowed,
          local_user_allowed
         ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(Config, [{rmq_nodename_suffix, ?MODULE}]),
    AuthConf =
        [{rabbit, [
             {auth_backends, [
                 {rabbit_auth_backend_internal, [rabbit_auth_backend_internal,
                                                 rabbit_auth_backend_ip_range]}
             ]}
        ]},
        {rabbitmq_auth_backend_ip_range, [
            {tag_masks,
                [{'ip-private', [<<"::FFFF:192.168.0.0/112">>]},
                 {'ip-local', [<<"::FFFF:127.0.0.1/112">>]}]},
            {default_masks, [<<"::0/128">>]}
        ]}
    ],
    Config2 = rabbit_ct_helpers:merge_app_env(Config1, AuthConf),
    Steps = rabbit_ct_broker_helpers:setup_steps() ++
            rabbit_ct_client_helpers:setup_steps(),
    Config3 = rabbit_ct_helpers:run_setup_steps(Config2, Steps),
    add_users(Config3),
    Config3.

add_users(Config) ->
    Pass = <<"pass">>,
    User0 = <<"local-user">>,
    Tags0 = ['ip-local'],
    ok = rabbit_ct_broker_helpers:add_user(Config, 0, User0, Pass),
    ok = rabbit_ct_broker_helpers:set_user_tags(Config, 0, User0, Tags0),
    ok = rabbit_ct_broker_helpers:set_full_permissions(Config, User0, <<"/">>),
    User1 = <<"private-user">>,
    Tags1 = ['ip-private'],
    ok = rabbit_ct_broker_helpers:add_user(Config, 0, User1, Pass),
    ok = rabbit_ct_broker_helpers:set_user_tags(Config, 0, User1, Tags1),
    ok = rabbit_ct_broker_helpers:set_full_permissions(Config, User1, <<"/">>).

end_per_suite(Config) ->
    Steps = rabbit_ct_client_helpers:teardown_steps() ++
            rabbit_ct_broker_helpers:teardown_steps(),
    rabbit_ct_helpers:run_teardown_steps(Config, Steps).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

guest_not_allowed(Config) ->
    User = <<"guest">>,
    Pass = <<"guest">>,
    {error, not_allowed} = rabbit_ct_client_helpers:open_unmanaged_connection(Config, 0, User, Pass).

private_user_not_allowed(Config) ->
    User = <<"private-user">>,
    Pass = <<"pass">>,
    {error, not_allowed} = rabbit_ct_client_helpers:open_unmanaged_connection(Config, 0, User, Pass).

local_user_allowed(Config) ->
    User = <<"local-user">>,
    Pass = <<"pass">>,
    case rabbit_ct_client_helpers:open_unmanaged_connection(Config, 0, User, Pass) of
        Conn when is_pid(Conn) ->
            ok = rabbit_ct_client_helpers:close_connection(Conn);
        Error ->
            error(Error)
    end.
