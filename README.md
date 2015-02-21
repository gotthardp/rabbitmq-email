# SMTP Gateway Plugin for RabbitMQ

This implementation aims to replace the [rabbitmq-smtp](https://github.com/rabbitmq/rabbitmq-smtp).
It is based on a more advanced [gen_smtp] (https://github.com/Vagabond/gen_smtp)
rather than on [erlang-smtp] (https://github.com/tonyg/erlang-smtp).

Current status: Functionality described in this document is fully implemented and
partially tested. I seek feature requests and early adopters.


## Mapping between SMTP and AMQP

The mapping works in both directions.

### SMTP to AMQP

The adapter listens for incoming emails. When an email arrives at the adapter,
its SMTP "To" address is examined to determine how it should be routed through
the system. First, the address is split into a mailbox name and a domain part.

 - the domain part (e.g. "`@rabbitmq.com`") is used to map to an
   AMQP virtual-host and AMQP exchange name
 - the mailbox name is mapped to an AMQP routing key

To receive the incoming emails, simply bind your queue(s) to this exchange. To
catch emails sent to unknown recipients you may use an
[Alternate Exchange](http://www.rabbitmq.com/ae.html).

To decrease message size and for security reasons the adapter may filter content
of each email published to the AMQP exchange. The adapter will:
 - pass to AMQP only selected MIME headers (Subject, Message-Id)
 - select only one part of `multipart/alternative` content
 - remove unknown, or vulnerable `multipart/mixed` body parts

### AMQP to SMTP

The adapter also consumes a set of AMQP queues. Each queue is linked with a
"default" domain name. When a message is consumed, its AMQP routing key is
examined to determine the target SMTP address.

 - routing key that includes a domain part (i.e. the "@" character") is mapped
   directly to an SMTP address
 - routing key that includes the mailbox name only (i.e. without "@") is combined
   with the "default" domain name assigned to the queue

To send emails, you should bind these queues to your exchange and then publish
a message to this exchange.

No content filtering is performed in this direction.


## Installation

### RabbitMQ Configuration
Add the plug-in configuration section. See
[RabbitMQ Configuration](https://www.rabbitmq.com/configure.html) for more details.

For example:
```erlang
{rabbitmq_email, [
    % gen_smtp server parameters
    % see https://github.com/Vagabond/gen_smtp#server-example
    {server_config, [
        [{port, 2525}, {protocol, tcp}, {domain, "example.com"}, {address,{0,0,0,0}}]
    ]},
    % inbound email exchanges: [{email-domain, {vhost, exchange}}, ...}
    {email_domains,
        [{<<"example.com">>, {<<"/">>, <<"email-in">>}}
    ]},

    % outbound email queues: [{{vhost, queue}, email-domain}, ...]
    {email_queues,
        [{{<<"/">>, <<"email-out">>}, <<"example.com">>}
    ]},
    % sender indicated in the From header
    {client_sender, "noreply@example.com"},
    % gen_smtp client parameters
    % see https://github.com/Vagabond/gen_smtp#client-example
    {client_config, [
        {relay, "smtp.example.com"}
    ]}
    ...
]}
```

### Postfix Integration
You may want to run a standard [Postfix](http://www.postfix.org) SMTP server on
the same machine and forward to the RabbitMQ plug-in only some e-mail domains.

- Edit `/etc/postfix/main.cf`
  - Add the domains to be processed by RabbitMQ to the `relay_domains` list

    ```
    relay_domains = $mydestination, example.com
    ```

  - Make sure the `transport_maps` file is enabled

    ```
    transport_maps = hash:/etc/postfix/transport
    ```

- Add links to the plug-in to the `/etc/postfix/transport` file

  ```
  example.com smtp:mail.example.com:2525
  .example.com smtp:mail.example.com:2525
  ```

### Installation from source

First, download and build
[RabbitMQ gen_smtp Integration](https://github.com/gotthardp/rabbitmq-gen-smtp).

To enable transcoding of incoming e-mails to a given character set build also
[RabbitMQ eiconv Integration](https://github.com/gotthardp/rabbitmq-eiconv).
This step is optional.

Then, build the main RabbitMQ plug-in. See the
[Plugin Development Guide](http://www.rabbitmq.com/plugin-development.html)
for more details.

    $ git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git
    $ cd rabbitmq-public-umbrella
    $ make co
    $ make BRANCH=<tag> up_c
    $ git clone https://github.com/gotthardp/rabbitmq-email.git
    $ cd rabbitmq-email
    $ make


## Copyright and Licensing

Copyright (c) 2014 Petr Gotthard <petr.gotthard@centrum.cz>

This package is subject to the Mozilla Public License Version 2.0 (the "License");
you may not use this file except in compliance with the License. You may obtain a
copy of the License at http://mozilla.org/MPL/2.0/.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
specific language governing rights and limitations under the License.
