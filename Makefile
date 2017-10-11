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

TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers amqp_client

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

NO_AUTOPATCH += eiconv

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

# --------------------------------------------------------------------
# Testing.
# --------------------------------------------------------------------

test-build:: test/system_SUITE_data/samples.zip

test/system_SUITE_data/samples.zip:
	$(gen_verbose) wget http://www.hunnysoft.com/mime/samples/samples.zip -O $@
	$(gen_verbose) unzip $@ -d $(@D) || true

.PHONY: delete-test/system_SUITE_data/samples.zip
delete-test/system_SUITE_data/samples.zip:
	$(gen_verbose) rm -rf system_SUITE_data/samples*

distclean:: delete-test/system_SUITE_data/samples.zip
	@:

include rabbitmq-components.mk
include erlang.mk
