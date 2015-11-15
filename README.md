# SMTP Gateway Plugin for RabbitMQ
Maps SMTP to AMQP (to convert an incoming email to an AMQP message) and AMQP
to SMTP (to send an email from an AMQP message).

This implementation aims to replace the [rabbitmq-smtp](https://github.com/rabbitmq/rabbitmq-smtp).
It is based on a more advanced [gen_smtp] (https://github.com/Vagabond/gen_smtp)
rather than on [erlang-smtp] (https://github.com/tonyg/erlang-smtp).

This plugin is experimental. The described functionality is fully implemented
and partially tested. I seek feature requests and early adopters.

If your installation of the plugin fails to process an e-mail, please:
 * re-send this e-mail to my instance of the plugin at 'rabbitmq[at]swimgate.eu'
 * create an [issue](https://github.com/gotthardp/rabbitmq-email/issues) and
   describe what you expected and what did happen

Please re-send the e-mail exactly the same way it failed for you. Do not use the
forward button, do not add any explanation. I need the original MIME headers and body.


## Mapping between SMTP and AMQP

The mapping works in both directions.

### AMQP to SMTP Conversion

The adapter consumes a set of AMQP queues. Each queue is linked with a
"default" domain name. When a message is consumed, its AMQP routing key is
examined to determine the target SMTP address.

 - routing key that includes a domain part (i.e. the "@" character") is mapped
   directly to an SMTP address
 - routing key that includes the mailbox name only (i.e. without "@") is combined
   with the "default" domain name assigned to the queue

To send emails, you should bind these queues to your exchange and then publish
a message to this exchange. The message gets converted as shown in the table
below. No content filtering is performed in this direction.

  AMQP                   | SMTP
 ------------------------|------------------------
                         | From: noreply@<domain>
  routing_key            | To
  message_id             | Message-Id
  content_type           | Content-Type
  headers                | additional headers


### SMTP to AMQP Conversion

The adapter listens for incoming emails. When an email arrives at the adapter,
its SMTP "To" address is examined to determine how it should be routed through
the system. First, the address is split into a mailbox name and a domain part.

 - the domain part (e.g. "`@rabbitmq.com`") is used to map to an
   AMQP virtual-host and AMQP exchange name
 - the mailbox name is mapped to an AMQP routing key

To receive the incoming emails, simply bind your queue(s) to this exchange. To
catch emails sent to unknown recipients you may use an
[Alternate Exchange](http://www.rabbitmq.com/ae.html).

When `server_auth` is `false` the server accepts e-mails from any client.
When `server_auth` is `rabbitmq` the clients need to provide a username
and password that is checked against the rabbitmq server.

#### SMTP Body Extraction

The `email_filter` configuration option can be used to extract information
from the email body. When enabled, the adapter will:
 - select only one part of `multipart` content depending on user priority;
 - remove extra space from `text` content.

The function is optional, to not extract anything and send the entire e-mail
as <<"application/mime">> set:

   ```erlang
   {email_filter, false}
   ```

Otherwise the `email_filter` identifies a list of content-types that shall
be preferred. For example:

 - Extract the text body or (when no text) the first binary attachement.
   This is the default behaviour.

   ```erlang
   {email_filter, [
     {<<"text">>, <<"plain">>},
     {<<"text">>, undefined},
     {undefined, undefined}
   ]}
   ```

   Each 2-tuple represents content type/subtype.
   The atom `undefined` represents any content other than <<"multipart">>.

 - Extract the first binary attachement or (when no attachement) the text body.

   ```erlang
   {email_filter, [
     {binary, undefined},
     {<<"text">>, <<"plain">>},
     {<<"text">>, undefined}
   ]}
   ```

   The atom `binary` represents any content other than <<"text">> and <<"multipart">>.

#### SMTP Headers Extraction

Depending on the `email_headers` option the message gets converted as shown in
the table below. The adapter will pass to AMQP only selected MIME headers

```erlang
{email_headers, ["subject", "from", "charset"]},
```

  SMTP                   | AMQP
 ------------------------|------------------------
  From                   |
  To                     | exchange, routing_key
                         | message_id
                         | timestamp
  Subject                | Subject
  Content-Type           | content_type


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
    % how clients are authenticated; either 'false' or 'rabbitmq' (default)
    {server_auth, rabbitmq},
    % whether STARTTLS shall be offered; either 'true' or 'false' (default)
    {server_starttls, true},
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

[![Build Status](https://travis-ci.org/gotthardp/rabbitmq-email.svg?branch=master)](https://travis-ci.org/gotthardp/rabbitmq-email)

To enable non-ASCII characters in e-mails build
[RabbitMQ eiconv Integration](https://github.com/gotthardp/rabbitmq-eiconv).
This step is optional; when eiconv is disabled the `gen_smtp` will ignore
both header and content encoding schemes.

Build and activate the RabbitMQ plug-in `rabbitmq-email`. See the
[Plugin Development Guide](http://www.rabbitmq.com/plugin-development.html)
for more details.

    $ git clone https://github.com/gotthardp/rabbitmq-email.git
    $ cd rabbitmq-email
    $ make

### History
* 0.1.0 (under development) Compatible with RabbitMQ 3.6.x and later.
* 0.0.2 (Nov 14, 2015)
  * Supports authentication using RabbitMQ database.
  * Payload filtering is now optional (Issue #7).
* 0.0.1 (May 12, 2015) First release.


## Copyright and Licensing

Copyright (c) 2014-2015 Petr Gotthard <petr.gotthard@centrum.cz>

This package is subject to the Mozilla Public License Version 2.0 (the "License");
you may not use this file except in compliance with the License. You may obtain a
copy of the License at http://mozilla.org/MPL/2.0/.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
specific language governing rights and limitations under the License.
