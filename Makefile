PROJECT = rabbitmq_email
PROJECT_DESCRIPTION = RabbitMQ plugin that converts incoming emails into messages and messages into outgoing emails

# use the patched gen_smtp from a fork
dep_gen_smtp = git https://github.com/gotthardp/gen_smtp.git master
dep_eiconv = git https://github.com/zotonic/eiconv.git master

BUILD_DEPS += gen_smtp

DEPS = gen_smtp rabbit_common amqp_client rabbit
ifeq ($(EICONV),1)
DEPS += eiconv
endif

TEST_DEPS = rabbit

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

NO_AUTOPATCH += eiconv

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk

# --------------------------------------------------------------------
# Testing.
# --------------------------------------------------------------------

samples: test/data/samples.zip tests

test/data/samples.zip:
	wget http://www.hunnysoft.com/mime/samples/samples.zip -O $@
	unzip $@ -d $(@D)

WITH_BROKER_TEST_COMMANDS := \
        eunit:test(rabbit_email_tests,[verbose,{report,{eunit_surefire,[{dir,\"test\"}]}}])
