PROJECT = rabbitmq_auth_backend_ip_range
PROJECT_DESCRIPTION = RabbitMQ IP Range Authentication Backend

define PROJECT_ENV
[
	{tag_masks, [{'ip-private', [<<"::FFFF:192.168.0.0/112">>]}]},
	{default_masks, [<<"::0/0">>]}
]
endef

LOCAL_DEPS = inets
DEPS = rabbit_common rabbit amqp_client
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
