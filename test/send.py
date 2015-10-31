#!/usr/bin/env python

import smtplib
from email.mime.text import MIMEText

me = "me@example.com"
you = "you@example.com"

msg = MIMEText("Hello world!")
msg['From'] = me
msg['To'] = you
msg['Subject'] = 'Hello'

s = smtplib.SMTP('localhost', 2525)
s.login("guest", "guest")
s.sendmail(me, [you], msg.as_string())
s.quit()
