# Changelog

## [v1.1.1](https://github.com/gotthardp/rabbitmq-email/tree/v1.1.1) (2022-10-21)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v1.1.0...v1.1.1)

**Closed issues:**

- Duplicated size statement in EHLO  [\#55](https://github.com/gotthardp/rabbitmq-email/issues/55)

**Merged pull requests:**

- Do not duplicate SIZE extension [\#56](https://github.com/gotthardp/rabbitmq-email/pull/56) ([lukebakken](https://github.com/lukebakken))

## [v1.1.0](https://github.com/gotthardp/rabbitmq-email/tree/v1.1.0) (2022-10-19)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v1.0.1...v1.1.0)

**Closed issues:**

- Unable to build anymore [\#52](https://github.com/gotthardp/rabbitmq-email/issues/52)
- Unable to send mail  [\#51](https://github.com/gotthardp/rabbitmq-email/issues/51)
- issue with specifying settings for gen\_ [\#47](https://github.com/gotthardp/rabbitmq-email/issues/47)

**Merged pull requests:**

- Allow setting max message size [\#54](https://github.com/gotthardp/rabbitmq-email/pull/54) ([lukebakken](https://github.com/lukebakken))
- Update for RabbitMQ 3.10, 3.11 [\#53](https://github.com/gotthardp/rabbitmq-email/pull/53) ([lukebakken](https://github.com/lukebakken))

## [v1.0.1](https://github.com/gotthardp/rabbitmq-email/tree/v1.0.1) (2022-03-09)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v1.0.0...v1.0.1)

**Closed issues:**

- Unable to send HTML email [\#50](https://github.com/gotthardp/rabbitmq-email/issues/50)
- plugin\_built\_with\_incompatible\_erlang [\#48](https://github.com/gotthardp/rabbitmq-email/issues/48)
- Enabling plugin support with later version of erl/23 [\#45](https://github.com/gotthardp/rabbitmq-email/issues/45)

**Merged pull requests:**

- Pin gen-smtp/gen\_smtp dep to 1.1.1 [\#49](https://github.com/gotthardp/rabbitmq-email/pull/49) ([lukebakken](https://github.com/lukebakken))

## [v1.0.0](https://github.com/gotthardp/rabbitmq-email/tree/v1.0.0) (2021-11-23)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v0.4.0...v1.0.0)

**Implemented enhancements:**

- Wild card email\_domains [\#24](https://github.com/gotthardp/rabbitmq-email/issues/24)
- Consider AMQP From/To headers when sending SMTP mails [\#20](https://github.com/gotthardp/rabbitmq-email/issues/20)

**Closed issues:**

- How to send email with attachment? [\#44](https://github.com/gotthardp/rabbitmq-email/issues/44)
- rabbitmq-server failed to start after enabling the rabbitmq\_email plugin [\#42](https://github.com/gotthardp/rabbitmq-email/issues/42)
- Config - New Format Style [\#40](https://github.com/gotthardp/rabbitmq-email/issues/40)
- Messages are stuck in queue? [\#32](https://github.com/gotthardp/rabbitmq-email/issues/32)
- How to create AMQP header if i want to send MIME header? [\#13](https://github.com/gotthardp/rabbitmq-email/issues/13)

**Merged pull requests:**

- Make this plugin compatible with RabbitMQ 3.9.x [\#46](https://github.com/gotthardp/rabbitmq-email/pull/46) ([lukebakken](https://github.com/lukebakken))
- Update .mk files from rabbitmq-common [\#43](https://github.com/gotthardp/rabbitmq-email/pull/43) ([lukebakken](https://github.com/lukebakken))

## [v0.4.0](https://github.com/gotthardp/rabbitmq-email/tree/v0.4.0) (2019-10-04)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v0.3.0...v0.4.0)

**Closed issues:**

- Can't enable plugin under Erlang 22 [\#38](https://github.com/gotthardp/rabbitmq-email/issues/38)
- Is it possible to route emiails based on subject? [\#37](https://github.com/gotthardp/rabbitmq-email/issues/37)
- Release 0.2.0 not compatible with Erlang 21 [\#35](https://github.com/gotthardp/rabbitmq-email/issues/35)

**Merged pull requests:**

- Fix typos in configuration properties [\#34](https://github.com/gotthardp/rabbitmq-email/pull/34) ([DennisHartrampf](https://github.com/DennisHartrampf))
- Fix typo in link in README [\#33](https://github.com/gotthardp/rabbitmq-email/pull/33) ([cmur2](https://github.com/cmur2))

## [v0.3.0](https://github.com/gotthardp/rabbitmq-email/tree/v0.3.0) (2019-04-03)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v0.2.0...v0.3.0)

**Closed issues:**

- Headers appearing as part of the email body, No subject [\#29](https://github.com/gotthardp/rabbitmq-email/issues/29)
- Using it on windows [\#28](https://github.com/gotthardp/rabbitmq-email/issues/28)
- Time for a new release? [\#27](https://github.com/gotthardp/rabbitmq-email/issues/27)
- Correct Installation of plugin [\#26](https://github.com/gotthardp/rabbitmq-email/issues/26)
- Problem building plugin  [\#25](https://github.com/gotthardp/rabbitmq-email/issues/25)

**Merged pull requests:**

- Release version 0.3.0 [\#36](https://github.com/gotthardp/rabbitmq-email/pull/36) ([lukebakken](https://github.com/lukebakken))
- Update to the latest build files [\#30](https://github.com/gotthardp/rabbitmq-email/pull/30) ([lukebakken](https://github.com/lukebakken))

## [v0.2.0](https://github.com/gotthardp/rabbitmq-email/tree/v0.2.0) (2017-10-05)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v0.1.0...v0.2.0)

**Fixed bugs:**

- Can't receive any email from google [\#14](https://github.com/gotthardp/rabbitmq-email/issues/14)
- Doesn't send email when no AMQP headers are defined [\#11](https://github.com/gotthardp/rabbitmq-email/issues/11)

**Closed issues:**

- Get full email headers in rabbitmq [\#23](https://github.com/gotthardp/rabbitmq-email/issues/23)
- error on sending email [\#22](https://github.com/gotthardp/rabbitmq-email/issues/22)
- Handling attachments [\#18](https://github.com/gotthardp/rabbitmq-email/issues/18)
- Create rabbitmq.config [\#16](https://github.com/gotthardp/rabbitmq-email/issues/16)
- Install it plugin [\#15](https://github.com/gotthardp/rabbitmq-email/issues/15)
- Crashes on receiving message [\#10](https://github.com/gotthardp/rabbitmq-email/issues/10)

**Merged pull requests:**

- Fix case mismatch [\#21](https://github.com/gotthardp/rabbitmq-email/pull/21) ([xtang](https://github.com/xtang))
- Set user as anonymous in EHLO when auth disable [\#12](https://github.com/gotthardp/rabbitmq-email/pull/12) ([xtang](https://github.com/xtang))

## [v0.1.0](https://github.com/gotthardp/rabbitmq-email/tree/v0.1.0) (2015-12-22)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v0.0.2...v0.1.0)

**Closed issues:**

- Error in Rabbit when passing emails [\#9](https://github.com/gotthardp/rabbitmq-email/issues/9)
- Can't run make [\#8](https://github.com/gotthardp/rabbitmq-email/issues/8)

## [v0.0.2](https://github.com/gotthardp/rabbitmq-email/tree/v0.0.2) (2015-11-14)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/v0.0.1...v0.0.2)

**Implemented enhancements:**

- security and raw message [\#7](https://github.com/gotthardp/rabbitmq-email/issues/7)

**Closed issues:**

- Unable to fetch 'from' [\#6](https://github.com/gotthardp/rabbitmq-email/issues/6)

## [v0.0.1](https://github.com/gotthardp/rabbitmq-email/tree/v0.0.1) (2015-05-12)

[Full Changelog](https://github.com/gotthardp/rabbitmq-email/compare/446a205d8cdddbedab1a25191b68e0679924c32e...v0.0.1)

**Fixed bugs:**

- Support  Multipart/Related Content-Type [\#4](https://github.com/gotthardp/rabbitmq-email/issues/4)
- error happens when client send an attache file to SMTP server [\#3](https://github.com/gotthardp/rabbitmq-email/issues/3)

**Closed issues:**

- error if incoming email is empty [\#5](https://github.com/gotthardp/rabbitmq-email/issues/5)
- Problem installing SMTP Gateway Plugin for RabbitMQ \[UBUNTU 14.04\] [\#1](https://github.com/gotthardp/rabbitmq-email/issues/1)

**Merged pull requests:**

- package.mk: Use `rabbitmq-gen-smtp` as the dependency name [\#2](https://github.com/gotthardp/rabbitmq-email/pull/2) ([dumbbell](https://github.com/dumbbell))
