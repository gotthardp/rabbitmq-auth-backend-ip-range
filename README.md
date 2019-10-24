# RabbitMQ plug-in for client authorization based on source IP address

[![Build Status](https://travis-ci.org/gotthardp/rabbitmq-auth-backend-ip-range.svg?branch=master)](https://travis-ci.org/gotthardp/rabbitmq-auth-backend-ip-range)

## Supported RabbitMQ and Erlang Versions

This plugin currently targets RabbitMQ 3.7.x and Erlang 20.3+.

## Binary Builds

Binary builds are published as [GitHub releases](https://github.com/gotthardp/rabbitmq-auth-backend-ip-range/releases).

## Configuration

You need to modify the
[rabbitmq.config](http://www.rabbitmq.com/configure.html#configuration-file).
An example configuration file follows:
```erlang
[
    {rabbit, [
        {auth_backends, [{rabbit_auth_backend_internal,
                          [rabbit_auth_backend_internal, rabbit_auth_backend_ip_range]
                         }]
        }
    ]},
    {rabbitmq_auth_backend_ip_range, [
        {tag_masks,
            [{'ip-private', [<<"::FFFF:192.168.0.0/112">>]}]},
        {default_masks, [<<"::0/0">>]}
    ]}
].
```
See [RabbitMQ Configuration](https://www.rabbitmq.com/configure.html) for more
details. The following sub-sections provide detailed explanation of the related
configuration options.

### Install new authorization backend

Add `rabbit_auth_backend_ip_range` to the list of `auth_backends`. RabbitMQ
allows you to define alternative authentication and authorication plug-ins.

You can say that modules `a1` **or** `a2` will be used to authenticate.
```erlang
{rabbit, [
    ...
    {auth_backends, [a1, a2]}
]},
```

You can also say that modules `z1` **and** `z2` will be used to authorize.
```erlang
{rabbit, [
    ...
    {auth_backends, [{a1, [z1, z2]}]}
]},
```

The `rabbit_auth_backend_ip_range` should be used for authorization only. It may
be used with the `rabbit_auth_backend_internal` or `rabbit_auth_backend_ldap`.

For example:
```erlang
{rabbit, [
    ...
    {auth_backends, [{rabbit_auth_backend_internal,
                      [rabbit_auth_backend_internal, rabbit_auth_backend_ip_range]
                     }]
    }
]},
```
This will use `rabbit_auth_backend_internal` for authentication. Authorization
will be done not only by `rabbit_auth_backend_internal`, but also and by
`rabbit_auth_backend_ip_range`.


### Setup the access control list

Add the `rabbitmq_auth_backend_ip_range` plug-in configuration section.

You may use the following parameters:

`tag_masks` --  List of tuples `{tag, [<<"ip/mask">>, ...]}`. The *tag*
corresponds to one of user [Tags](https://www.rabbitmq.com/management.html#permissions);
the *ip/mask* is a permitted network mask of remote (client) address.

`default_masks` -- Default IP network mask `[<<"ip/mask">>, ...]` used when the
user has none of the listed tags. Set this to `[<<"::0/0">>]` to accept untagged
users (default behaviour), or to `[<<"::0/127">>]` to reject untagges users.

For example:
```erlang
{rabbitmq_auth_backend_ip_range, [
    {tag_masks,
        [{'ip-private', [<<"::FFFF:192.168.0.0/112">>]}]},
    {default_masks, [<<"::0/0">>]}
]},
```
This will allow users with the tag `ip-private` to login from private networks
only. Other users will be able to login from any network.

The IPv4 mask (e.g. 192.168.0.0) will match an IPv4 address only.
The IPv4-mapped IPv6 mask (e.g. ::FFFF:192.168.0.0) will match both IPv4 and
IPv6 addresses.


## Build Instructions

[![Build Status](https://travis-ci.org/gotthardp/rabbitmq-auth-backend-ip-range.svg?branch=master)](https://travis-ci.org/gotthardp/rabbitmq-auth-backend-ip-range)

This plug-in requires RabbitMQ 3.6.0, or higher. Build the plug-in following the
standard [Plugin Development Guide](https://www.rabbitmq.com/plugin-development.html).

## History

Version | Date         | Erlang |  Notes
------- |------------- | -------| -----------------------
`1.0.0` | `2019-10-24` | `20.3` | Compatible with RabbitMQ 3.7.14 or later
`3.7.14`| `2019-04-03` | `20.3` | Tag and release are deleted, use version `1.0.0`
`3.7.0` | `2018-08-01` | `19.3` | Compatible with RabbitMQ 3.7.x. Use [this commit](https://github.com/gotthardp/rabbitmq-auth-backend-ip-range/commit/44061917c49d67bbeded6ba1b4370d98ff680215) and build with Erlang 19.3 if you need this version
`0.2.0` | `2015-12-22` |        | Compatible with RabbitMQ 3.6.x. Implement RabbitMQ [Issue 109](https://github.com/rabbitmq/rabbitmq-server/issues/109) fix for authorization of MQTT/STOMP connections.
`0.1.1` | `2015-10-27` |        | Bugfix release.<ul><li>IPv4-mapped IPv6 addresses now match the respective IPv4 address.</li><li>Added some debug logs to hunt the Issue #2.</li><li>Authenticate against remote (client) address instead of local (server) address.</li></ul>
`0.1.0` | `2014-11-14` |        | First release. Compatible with RabbitMQ 3.5.x only.
