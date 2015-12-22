PROJECT = rabbitmq_auth_backend_ip_range

DEPS = amqp_client

TEST_DEPS = rabbit

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk

# --------------------------------------------------------------------
# Testing.
# --------------------------------------------------------------------

WITH_BROKER_SETUP_SCRIPTS := $(CURDIR)/test/setup-rabbit-test.sh
WITH_BROKER_TEST_MAKEVARS := \
        RABBITMQ_CONFIG_FILE=$(CURDIR)/test/rabbit-test
WITH_BROKER_TEST_COMMANDS := \
        eunit:test(rabbit_auth_tests,[verbose,{report,{eunit_surefire,[{dir,\"test\"}]}}])
