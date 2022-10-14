PROJECT = rabbitmq_email
PROJECT_DESCRIPTION = RabbitMQ plugin that converts incoming emails into messages and messages into outgoing emails
RABBITMQ_VERSION ?= v3.11.x

# Note: must be the same version as in rabbitmq-components.mk
DEP_RANCH_VERSION = 2.1.0

dep_ranch = ranch $(DEP_RANCH_VERSION)
dep_eiconv = hex 1.0.0
dep_gen_smtp = git https://github.com/gen-smtp/gen_smtp.git 1.1.1

DEPS = gen_smtp rabbit_common amqp_client rabbit eiconv

TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers amqp_client eiconv

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

# --------------------------------------------------------------------
# Testing.
# --------------------------------------------------------------------

test-build:: test/system_SUITE_data/samples.zip

test/system_SUITE_data/samples.zip:
	$(gen_verbose) curl -L http://www.hunnysoft.com/mime/samples/samples.zip --output $@
	$(gen_verbose) unzip $@ -d $(@D) || true

.PHONY: delete-test/system_SUITE_data/samples.zip
delete-test/system_SUITE_data/samples.zip:
	$(gen_verbose) rm -rf system_SUITE_data/samples*

distclean:: delete-test/system_SUITE_data/samples.zip
	@:

include rabbitmq-components.mk
include erlang.mk

autopatch-gen_smtp::
	$(verbose) sed -i.autopatch.bak -e 's/dep_ranch[^=]*=.*ranch$$/dep_ranch = hex $(DEP_RANCH_VERSION) ranch/' $(DEPS_DIR)/gen_smtp/Makefile
