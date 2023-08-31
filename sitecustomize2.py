__license__ = '''\
# Copyright (c) 2011 Edward Langley
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# Neither the name of the project's author nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.'''

import re
import functools
import os.path
import inspect
def inst(x): return x()
@inst
class __Null(object): pass
def getTerminalSize():
    def ioctl_GWINSZ(fd):
        try:
            import fcntl, termios, struct, os
            cr = struct.unpack('hh', fcntl.ioctl(fd, termios.TIOCGWINSZ, '1234'))
        except:
            return None
        return cr
    cr = ioctl_GWINSZ(0) or ioctl_GWINSZ(1) or ioctl_GWINSZ(2)
    if not cr:
        try:
            fd = os.open(os.ctermid(), os.O_RDONLY)
            cr = ioctl_GWINSZ(fd)
            os.close(fd)
        except:
            pass
    if not cr:
        try:
            cr = (env['LINES'], env['COLUMNS'])
        except:
            cr = (25, 80)
    return int(cr[1]), int(cr[0])


def cmp(a,b): return (a > b) - (a < b)

def sort_(nms):
	return sorted(nms, key = cmp_to_key(lambda x,y: cmp(x.lower().lstrip('_').rpartition('__')[2], y.lower().lstrip('_').rpartition('__')[2])))

def sort_decorator(func):
	@functools.wraps(func)
	def _inner(*a, **kw):
		result = func(*a, **kw)
		return sort_(result)
	return _inner

def pprint_decorator(func):
	@functools.wraps(func)
	def _inner(*args, **kwargs):
		result = func(*args, **kwargs)
		return pprint(result)
	return _inner

def dir_decorator(func):
    @functools.wraps(func)
    def _inner(nm=__Null, match=__Null):
            print(get_interpframe())
            result = None
            if nm is __Null: result = get_interpframe().f_globals.keys()
            else:
                    result = func(nm)
                    if match is not __Null:
                            x = [x for x in result if x.startswith(match)]
                            if x: result = x
                            else:
                                    x = [x for x in result if x.endswith(match)]
                                    if x: result = x
                                    else:
                                            omatch = match
                                            if not hasattr(match, 'match'): match = re.compile(match)
                                            x = [x for x in result if match.match(x)]
                                            if x: result = x
                                            else:
                                                    raise ValueError('No symbols match %s' % omatch)
                            if x: result = x
            return result
    return _inner

def _pprint(nms):
	length = max(len(s) for s in nms)
	count = 0
	cc = 1
	while (cc*length) < ((getTerminalSize()[0])-2*cc):
		cc+=1
	cc -= 1
	if cc <= 0: cc = 1
	for v, c in enumerate(nms):
		print(c.strip().ljust(length),end='')
		if v % cc == cc-1: print()

pprint = _pprint

try:
	__builtins__['pdir'] = pprint_decorator(sort_decorator(dir_decorator(dir)))
	__builtins__['fwprint'] = pprint
except:
	__builtins__.pdir = pprint_decorator(sort_decorator(dir_decorator(dir)))
	__builtins__.fwprint = pprint

try:
	import rlcompleter
	import readline
except ImportError:
	pass

readline.parse_and_bind ("tab complete")

import tempfile, subprocess
def get_interpframe(): return inspect.getouterframes(inspect.currentframe())[-1][0]

def e(name=None):
	def load_file():
		result = tempfile.NamedTemporaryFile(delete=False, dir=os.path.join(os.environ['HOME'], 'sandbox','unsorted'), prefix='pythonsnippets_', suffix='.py')
		print >>result, __license__, '\n\n'
		result.flush()
		return result

	if name is not None:
		def load_file():
			existed = True

			nm = os.path.join(os.environ['HOME'], 'sandbox', '%s.py' % name)
			mode = 'r+'
			if not os.path.exists(nm):
				mode = 'w'
				existed = False

			result = open(nm, mode)
			if not existed:
				print >>result, __license__, '\n\n'
				result.flush()
			return result

	f = load_file()
	print ('editing %s, press Enter to continue...' % f.name)

	try:
		try: subprocess.call(['vim', '+31', r'+start', f.name])
		except Exception, e: print e
		f.close()
		f = open(f.name)
		a = f.read()
		exec a in get_interpframe().f_globals
	finally:
		f.close()

def ls(name):
	load_file = lambda: open('/Users/edwlan/sandbox/unsorted/pythonsnippets_%s.py' % name)
	try:
		f = load_file()
	except IOError:
		load_file = lambda: open('/Users/edwlan/sandbox/%s.py' % name)
		f = load_file()

	raw_input('loading %s, press Enter to continue...' % f.name)
	try:
		exec f.read() in get_interpframe().f_globals
	finally:
		f.close()


try:
	__builtins__['e'] = e
	__builtins__['ls'] = ls
except:
	__builtins__.e = e
	__builtins__.ls = ls

#histfile = os.path.join(os.environ["HOME"], ".pyhist2.7")
#try: readline.read_history_file(histfile)
#except IOError: pass

try:
	import pip
	try:
		def ip(name):
			try: pip.main(['install', name])
			except SystemExit: pass
		__builtins__['ip'] = ip
	except:
		__builtins__.ip = ip
except ImportError:
	pass


#import atexit
#atexit.register(readline.write_history_file, histfile)
