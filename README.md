# RabbitMQ plug-in for client authorization based on source IP address

## Configuration

### Install new authorization backend

Add `rabbit_auth_backend_ip_range` to the list of `auth_backends`. RabbitMQ
allows you to define alternative authentication and authorication plug-ins.

You can say that modules `a1` **or** `a2` will be used to authenticate.
```
{rabbit, [
    ...
    {auth_backends, [a1, a2]}
]},
```

You can also say that modules `z1` **and** `z2` will be used to authorize.
```
{rabbit, [
    ...
    {auth_backends, [{a1, [z1, z2]}]}
]},
```

The `rabbit_auth_backend_ip_range` should be used for authorization only. It may
be used with the `rabbit_auth_backend_internal` or `rabbit_auth_backend_ldap`.

For example:
```
{rabbit, [
    ...
    {auth_backends, [{rabbit_auth_backend_internal, [rabbit_auth_backend_internal, rabbit_auth_backend_ip_range]}]}
]},
```
This will use `rabbit_auth_backend_internal` for authentication. Authorization
will be done not only by `rabbit_auth_backend_internal`, but also and by
`rabbit_auth_backend_ip_range`.


### Setup the access control list

Add the plug-in configuration section. See
[RabbitMQ Configuration](https://www.rabbitmq.com/configure.html) for more details.

You may use the following parameters:

`tag_masks` --  List of tuples `{tag, [<<"ip/mask">>, ...]}`. The *tag*
corresponds to user Tags; the *ip/mask* is a permitted network mask.

`default_masks` -- Default IP network mask `[<<"ip/mask">>, ...]` used when the
user has none of the listed tags. Set this to `[<<"::0/0">>]` to accept untagged
users (default behaviour), or to `[<<"::0/127">>]` to reject untagges users.

For example:
```
{rabbitmq_auth_backend_ip_range, [
    {tag_masks,
        [{'ip-private', [<<"::FFFF:192.168.0.0/112">>]}]},
    {default_masks, [<<"::0/0">>]}
]},
```
This will allow users with the tag `ip-private` to login from private networks
only. Other users will be able to login from any network.


## Build Instructions

Download a patched RabbitMQ from [GitHub](https://github.com/gotthardp/rabbitmq-server/tree/improved_auth).

Build the plug-in following the standard [Plugin Development Guide](https://www.rabbitmq.com/plugin-development.html).
