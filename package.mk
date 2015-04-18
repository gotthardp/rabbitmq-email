RELEASABLE:=true
RETAIN_ORIGINAL_VERSION:=true
DEPS:=rabbitmq-server rabbitmq-erlang-client rabbitmq-gen-smtp

WITH_BROKER_TEST_COMMANDS:=eunit:test(rabbit_email_tests,[verbose])
