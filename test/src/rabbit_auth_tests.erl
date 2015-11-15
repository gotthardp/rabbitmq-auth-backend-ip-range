%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_auth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

auth_test_() ->
    % default mask
    [?_assertEqual({error,not_allowed}, connect(<<"guest">>, <<"guest">>)),
    ?_assertEqual({error,not_allowed}, connect(<<"private-user">>, <<"pass">>)),
    ?_assertEqual(ok, connect(<<"local-user">>, <<"pass">>))].

connect(User, Pass) ->
    case amqp_connection:start(#amqp_params_network{username=User,
                                                    password=Pass}) of
        {ok, Connection} ->
            amqp_connection:close(Connection),
            ok;
        {error,{auth_failure, _Message}} ->
            {error, auth_failure};
        {error, Error} ->
            {error, Error}
    end.

% end of file
