#!/usr/bin/python

import httplib
import re
import time

def check_site(address,domain,url='/',datamatch=''):
	c = httplib.HTTPConnection(address)
	c.request('GET', url, headers={'Host': domain})
	r1 = c.getresponse()
	if r1.status!=200:
		raise Exception('%d: %s' % (r1.status,r1.reason))
	data = r1.read()
	if not re.search(datamatch,data):
		raise Exception('The data retrieved was wrong or incomplete.')
	c.close()
	return data.strip()

f = open('monlog','a')
period = 6
while True:
	now = time.asctime(time.localtime(time.time()))
	try:
		d = check_site('64.124.162.90','classicgames.org',url='/test.cgi',datamatch=r'angst')
		f.write('%s success (%s)\n' % (now,d))
	except Exception, inst:
		er = str(inst)
		if not er:
			er = 'timeout?'
		f.write('%s failure (%s)\n' % (now,er))
	f.flush()
	time.sleep(period-time.localtime(time.time())[5]%period)
