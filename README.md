# SMTP Gateway Plugin for RabbitMQ

This implementation  aims to replace the https://github.com/rabbitmq/rabbitmq-smtp.
It is based on a more advanced [gen_smtp] (https://github.com/Vagabond/gen_smtp)
rather than on [erlang-smtp] (https://github.com/tonyg/erlang-smtp).


## Mapping between SMTP and AMQP

The adapter listens for incoming emails. When an email arrives at the adapter,
its SMTP "To" address is examined to determine how it should be routed through
the system. First, the address is split into a mailbox name and a domain part.

 - the domain part (e.g. "`@rabbitmq.com`") is used to map to an
   AMQP virtual-host and AMQP exchange name
 - the mailbox name is mapped to an AMQP routing key

The adapter also binds to a set of AMQP exchanges; each binding has a consumer
tag that maps to a "default" domain name. When a message is consumed, its AMQP
routing key is examined to determine the target SMTP address.

 - routing key that includes the domain part (i.e. the "@" character") is mapped
   directly to an SMTP address
 - routing key that includes the mailbox name only is combined with the domain
   name obtained from the consumer tag


## Configuration
Add the plug-in configuration section. See
[RabbitMQ Configuration](https://www.rabbitmq.com/configure.html) for more details.

For example:
```
{rabbitmq_email, [
    % email to amqp configuration
    {server_config, [
        [{port, 2525}, {protocol, tcp}, {domain, "example.com"}, {address,{0,0,0,0}}]
    ]},
    {email_domains,
        [{<<"example.com">>, {<<"/">>, <<"email-in">>}}
    ]},

    % amqp to email configuration
    {email_queues,
        [{{<<"/">>, <<"email-out">>}, <<"example.com">>}
    ]},
    {client_sender, "noreply@example.com"},
    {client_config, [
        {relay, "smtp.example.com"}
    ]}
    ...
]}
```

## Copyright and Licensing

Copyright (c) 2014 Petr Gotthard <petr.gotthard@centrum.cz>

This package is subject to the Mozilla Public License Version 2.0 (the "License");
you may not use this file except in compliance with the License. You may obtain a
copy of the License at http://mozilla.org/MPL/2.0/.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
specific language governing rights and limitations under the License.
