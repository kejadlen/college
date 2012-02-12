#!/usr/bin/python

import math
import re
import sys

logfile = sys.argv[1]
dmatch = ' '.join(sys.argv[2:])
hits,misses = 0,0
f = open(logfile,'r')
for jo in f.readlines():
	if re.search(dmatch,jo):
		hits += 1
		if re.search(r'fail',jo):
			misses += 1
f.close()

if hits < 1:
	print 'No data'
else:
	nines = '+inf'
	if misses > 0:
		nines = '%.4f' % -math.log10(float(misses)/float(hits))
	print '%d failures in %d attempts for %.4f%% uptime (%s nines)' % (misses,hits,float(100.0*(hits-misses))/float(hits),nines) 
