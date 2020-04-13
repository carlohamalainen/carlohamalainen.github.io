# -*- coding: utf-8 -*-

"""

Send each tweet in my friend list to email!

Author: Carlo Hamalainen <carlo.hamalainen@gmail.com>

Version: 2010-10-26


Requires the 'twurl' program.

(1) sudo aptitude install ruby-full

(2) Visit http://packages.ubuntu.com/maverick/all/rubygems1.8/download
and download rubygems1.8_1.3.7-2_all.deb

(3) sudo dpkg --install Downloads/rubygems1.8_1.3.7-2_all.deb

(4) sudo gem i twurl --source http://rubygems.org

(5) Go to http://dev.twitter.com/apps and create a new application. Get
the consumer key and secret consumer key.

(6) Now authenticate with twurl (you only have to do this once):

twurl authorize -u YOUR_USERNAME -p YOUR_PASSWORD -c YOUR_CONSUMER_KEY -s YOUR_SECRET_CONSUMER_KEY

(7) python tweet2email.py

"""

import codecs
import os
import sys
import xml.parsers.expat

TWURL_BINARY = 'twurl'
TWURL_CONSUMER_KEY = 'fixme'
TWURL_CONSUMER_SECRET_KEY = 'fixme'

def send(subject, body):
    import smtplib

    from email.mime.multipart import MIMEMultipart
    from email.mime.text import MIMEText

    me = "foo@example.com"
    you = "blah@example.com"

    msg = MIMEMultipart('alternative')
    msg.set_charset('UTF-8')

    msg['Subject'] = ''.join([c for c in subject if ord(c) < 128]) # messy!

    msg['From'] = me
    msg['To'] = you

    text = body

    part1 = MIMEText(text.encode('UTF-8'), 'text')
    part2 = MIMEText(text.encode('UTF-8'), 'html')

    msg.attach(part1)
    msg.attach(part2)

    s = smtplib.SMTP('localhost')
    s.sendmail(me, you, msg.as_string())
    s.quit()

    print 'Sent: %s' % msg['Subject']


def tweet_xml_to_email(blah):
    s = u''

    s += u'%s<br><br>http://twitter.com/%s: %s' % (blah['created_at'], blah['screen_name'], blah['tweet_text'])
    s += u'<br><br>\n'
    s += u'<a href="http://twitter.com/?status=@%s&in_reply_to_status_id=%s&in_reply_to=%s">reply</a>' % (blah['screen_name'], blah['tweet_id'], blah['screen_name'])

    return (u'[tweet2email] %s: %s' % (blah['screen_name'], blah['tweet_text'].replace('\n', ' ')), s)

def start_element(name, attrs):
    global current_mode, mode_stack, stuff

    if current_mode is None or current_mode != name:
        mode_stack.append(name)
        current_mode = name

    stuff = u''

def end_element(name):
    global current_mode, mode_stack, stuff, blah, tmp_file

    if mode_stack == [u'statuses', u'status', u'user', u'screen_name']:
        blah['screen_name'] = stuff

    if mode_stack == [u'statuses', u'status', u'created_at']:
        blah['created_at'] = stuff

    if mode_stack == [u'statuses', u'status', u'user', u'id']:
        blah['user_id'] = stuff

    if mode_stack == [u'statuses', u'status', u'text']:
        blah['tweet_text'] = stuff

    if mode_stack == [u'statuses', u'status', u'id']:
        blah['tweet_id'] = stuff

    if mode_stack == [u'statuses', u'status'] and name == 'status':
        out = open(tmp_file, 'wb')
        sub, body = tweet_xml_to_email(blah)
        out.write(body.encode("utf-8"))
        out.close()

        tweet_filename = 'sent_tweets/' + blah['tweet_id']
        if not os.path.isfile(tweet_filename):
            send(sub, body)
            os.system('touch ' + tweet_filename)

    assert mode_stack[-1] == name
    mode_stack.pop()
    current_mode = None if mode_stack == [] else mode_stack[-1]

def char_data(data):
    global current_mode
    global mode_stack
    global stuff

    stuff += data

if __name__ == '__main__':
    cmd = '%s /statuses/home_timeline.xml?count=20' % (TWURL_BINARY,)
    child_stdin, child_stdout, child_stderr = os.popen3(cmd)

    print cmd

    home_timeline = child_stdout.read()
    assert child_stderr.read() == ''

    # These three variables are modified by the functions
    # start_element, end_element, and char_data.
    current_mode = None
    mode_stack = []
    blah = {}

    tmp_file = os.popen('mktemp').read().rstrip()

    p = xml.parsers.expat.ParserCreate(encoding="UTF-8")

    p.StartElementHandler = start_element
    p.EndElementHandler = end_element
    p.CharacterDataHandler = char_data

    p.Parse(home_timeline)

    os.remove(tmp_file)
 
