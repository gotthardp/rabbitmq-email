# SMTP Gateway Plugin for RabbitMQ

This plugin makes SMTP and AMQP 0-9-1 interoperate. It can

 * convert incoming emails to AMQP 0-9-1 messages
 * consume AMQP 0-9-1 messages and send out emails

Certain interoperability with other protocols, namely STOMP, can be achieved as well.

This implementation aims to replace the [rabbitmq-smtp](https://github.com/rabbitmq/rabbitmq-smtp) plugin.
It is based on a more advanced [gen_smtp] (https://github.com/Vagabond/gen_smtp)
rather than on [erlang-smtp](https://github.com/tonyg/erlang-smtp).

This plugin is moderately mature. The described functionality is fully implemented
and has been used in production for a couple of years. Feedback from users and test suite
contributions are encouraged.


## Installation

Binary releases of this plugin are [published on GitHub](https://github.com/gotthardp/rabbitmq-email/releases).

## Documentation

The mapping between SMTP and AMQP 0-9-1 works in both directions. Before we provide a specific
configuration example, let's take a look at the conceptual mapping between messages in the two protocols.

### AMQP 0-9-1 to SMTP Conversion Workflow

The adapter consumes a set of AMQP queues (e.g. `email-out`). Each queue is linked
with a "default" domain name. When a message is consumed, its AMQP routing key is
examined to determine the target SMTP address.

 - routing key that includes a domain part (i.e. the "@" character") is mapped
   directly to an SMTP address
 - routing key that includes the mailbox name only (i.e. without "@") is combined
   with the "default" domain name assigned to the queue

To send emails, you should bind these queues to your exchange and then publish
a message to this exchange. For example:
```python
import pika

connection = pika.BlockingConnection(
    pika.ConnectionParameters(host='localhost'))
channel = connection.channel()
channel.exchange_declare(exchange='X', type='topic')
channel.queue_bind(exchange='X', queue='email-out', routing_key='#')

channel.basic_publish(exchange='X',
    routing_key='recipient@example.com',
    properties=pika.BasicProperties(
        content_type = 'text/plain',
        headers = {'Subject':'Greetings'}),
    body='Hello world!')
connection.close()
```
The message gets converted as shown in the table below. No content filtering
is performed in this direction.

  AMQP                   | SMTP
 ------------------------|------------------------
                         | From: noreply@<domain>
  routing_key            | To
  message_id             | Message-Id
  content_type           | Content-Type
  headers                | additional headers


### SMTP to AMQP 0-9-1 Conversion Workflow

The adapter listens for incoming emails. When an email arrives at the adapter,
its SMTP "To" address is examined to determine how it should be routed through
the system. First, the address is split into a mailbox name and a domain part.

 - the domain part (e.g. "`@rabbitmq.com`") is used to map sender's address to a
   RabbitMQ virtual host and exchange name
 - the mailbox name is mapped to an AMQP 0-9-1 routing key

To receive the incoming emails, simply bind your queue(s) to this exchange.
For example:
```python
import smtplib
from email.mime.text import MIMEText

me = "sender@example.com"
you = "recipient@example.com"

msg = MIMEText("Hello world!")
msg['From'] = me
msg['To'] = you
msg['Subject'] = 'Greetings'

s = smtplib.SMTP('localhost', 2525)
s.login("guest", "guest")
s.sendmail(me, [you], msg.as_string())
s.quit()
```
To catch emails sent to unknown recipients you may use an
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



### RabbitMQ Configuration Example

Like with most plugins, this one needs a configuration section in the RabbitMQ config file.
[RabbitMQ Configuration guide](https://www.rabbitmq.com/configure.html) for more details.

All keys used by this plugin are under the `rabbitmq_email` section (app). Key settings are:

 * `rabbitmq_email.server_config` defines SMTP server parameters (same as in `gen_smtp`)
 * `rabbitmq_email.email_domains` maps sender domains to RabbitMQ [virtual hosts and exchanges](http://www.rabbitmq.com/tutorials/amqp-concepts.html)
 * `rabbitmq_email.emal_queues` maps [virtual hosts and queues](http://www.rabbitmq.com/tutorials/amqp-concepts.html) to outgoing email domains
 * `rabbitmq_email.client_config` configures SMTP client settings for outgoing email
 * `rabbitmq_email.emal_from` and `rabbitmq_email.client_sender` configure the `FROM` header used in outgoing emails

For example:

```erlang
{rabbitmq_email, [
    %% gen_smtp server parameters
    %% see https://github.com/Vagabond/gen_smtp#server-example
    {server_config, [
        [{port, 2525}, {protocol, tcp}, {domain, "example.com"}, {address,{0,0,0,0}}]
    ]},
    %% how clients are authenticated; either 'false' or 'rabbitmq' (default)
    {server_auth, rabbitmq},
    %% whether STARTTLS shall be offered; either 'true' or 'false' (default)
    {server_starttls, true},

    %% maps inbound email domains to vhosts and exchanges: [{email-domain, {vhost, exchange}}, ...}
    {email_domains,
        [{<<"example.com">>, {<<"/">>, <<"email-in">>}}
    ]},

    %% outbound email queues: [{{vhost, queue}, email-domain}, ...]
    {email_queues,
        [{{<<"/">>, <<"email-out">>}, <<"example.com">>}
    ]},
    %% sender indicated in the From header
    {email_from, <<"noreply">>},
    %% sender indicated in the SMTP from
    {client_sender, "rabbitmq@example.com"},
    %% gen_smtp client parameters
    %% see https://github.com/Vagabond/gen_smtp#client-example
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

## Getting Help

In case the plugin doesn't seem to work as expected, please start a
[rabbitmq-users](https://groups.google.com/group/rabbitmq-users/)
thread and provide a way to reproduce:

 * RabbitMQ version used
 * Plugin version used
 * Steps to reproduce
 * Server logs

If the email => message workflow is used, please provide the exact
email used. Do not use the forward button or modify the content in any way.
Original MIME headers and body are critically important in troubleshooting.


### Building from Source

You can build and install it like any other plugin (see
[the plugin development guide](http://www.rabbitmq.com/plugin-development.html)).

To enable non-ASCII characters in e-mails, export `EICONV=1` and run `make dist`:

    EICONV=1 make dist

This is optional as it requires an Erlang NIF, `eiconv`. It is built
automatically, but since this is a NIF (native code) its module
(eiconv-1.1.ez) is not portable between platforms.  When `eiconv` is
disabled the `rabbitmq-email` plugin will ignore both header and
content encoding schemes.

### Change Log

* 0.1.0 (Dec 22, 2015)
  * Compatibility changes for RabbitMQ 3.6.x.
* 0.0.2 (Nov 14, 2015)
  * Supports authentication using RabbitMQ database.
  * Payload filtering is now optional (Issue #7).
* 0.0.1 (May 12, 2015) First release.


## Copyright and Licensing

Copyright (c) 2014-2017 Petr Gotthard <petr.gotthard@centrum.cz>
Copyright (c) 2017 Pivotal Software, Inc.

This package is subject to the Mozilla Public License Version 2.0 (the "License");
you may not use this file except in compliance with the License. You may obtain a
copy of the License at http://mozilla.org/MPL/2.0/.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
specific language governing rights and limitations under the License.
