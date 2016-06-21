#! /usr/bin/env python


"""
An example IRC log bot - logs a channel's events to a file.

If someone says the bot's name in the channel followed by a ':',
e.g.

    <foo> logbot: hello!

the bot will reply:

    <logbot> foo: I am a log bot

Run this script with two arguments, the channel name the bot should
connect to, and file to log to, e.g.:

    $ python ircLogBot.py test test.log

will log channel #test to the file 'test.log'.

To run the script:

    $ python ircLogBot.py <channel> <file>
"""


# twisted imports
from twisted.words.protocols import irc
from twisted.internet import reactor, protocol, threads
from twisted.python import log
import base64

# system imports
import time, sys


class MessageLogger:
    """
    An independent logger class (because separation of application
    and protocol logic is a good thing).
    """
    def __init__(self, bot):
        self.bot = bot
        self.user_queue = []

    def get_message(self):
        """Write a message to the file."""
        try:
            inp = raw_input('irc> ')
            return map(str.strip, inp.partition(':')[::2])
        except (EOFError, KeyboardInterrupt):
            reactor.callFromThread(reactor.stop)
            return (None,None)

    def send_message(self, res):
        user, message = res
        if user is not None:
            print '<%s> %s: %s' % (self.bot.nickname, user, message)
            self.user_queue.append(user)
            reactor.callFromThread(self.bot.msg, user, message)

    def handle(self, user, message):
        handled = self.user_queue != [] and self.user_queue.pop(0) == user
        if handled:
            print '\r<%s> %s: %s' % (user, self.bot.nickname, message)
        return handled


class LogBot(irc.IRCClient):
    """A logging IRC bot."""

    nickname = base64.encodestring('fiddlerwoaroof')[:len('fiddlerwoaroof')]

    def connectionMade(self):
        irc.IRCClient.connectionMade(self)
        self.join('#lisp');

        self.handler = MessageLogger(self)
        d = threads.deferToThread(self.handler.get_message)
        d.addCallback(self.handler.send_message)
        reactor.callLater(5, self.changeNick)

    def changeNick(self):
        self.setNick(base64.encodestring(self.nickname)[:len(self.nickname)].strip())
        reactor.callLater( (random.random()-0.5)*2.5 + 5, self.changeNick )

    def privmsg(self, user, channel, msg):
        """This will get called when the bot receives a message."""
        user = user.split('!', 1)[0]

        # Check to see if they're sending me a private message
        if channel == self.nickname:
            handled = self.handler.handle(user, msg)
            if handled:
                d = threads.deferToThread(self.handler.get_message)
                d.addCallback(self.handler.send_message)

    # For fun, override the method that determines how a nickname is changed on
    # collisions. The default method appends an underscore.
    def alterCollidedNick(self, nickname):
        """
        Generate an altered version of a nickname that caused a collision in an
        effort to create an unused related name for subsequent registration.
        """
        return base64.encodestring(nickname)[:len(nickname)].strip()



class LogBotFactory(protocol.ClientFactory):
    """A factory for LogBots.

    A new protocol instance will be created each time we connect to the server.
    """

    def buildProtocol(self, addr):
        p = LogBot()
        p.factory = self
        return p

    def clientConnectionLost(self, connector, reason):
        """If we get disconnected, reconnect to server."""
        connector.connect()

    def clientConnectionFailed(self, connector, reason):
        print "connection failed:", reason
        reactor.stop()


if __name__ == '__main__':
    # create factory protocol and application
    f = LogBotFactory()

    # connect factory to this host and port
    reactor.connectTCP("irc.freenode.net", 6667, f)

    # run bot
    reactor.run()
