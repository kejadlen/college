#!/usr/bin/npython

import Cookie
import cgi
import MySQLdb
import naccount
import nethernet
import npricer
import os
import pickle
import random
import re
import tempfile
import time

import sys
import urllib

import cgitb
cgitb.enable()

_VERSION_MAJOR = 2
_VERSION_MINOR = 0
_VERSION_SUBMINOR = 19

execfile('strings/strings.en.py')

templatedir = 'templates'
expand = { 'content': '', 'sidelinks': '', 'title': '', 'fmh1': '', 'fmh2': '' }
dompath = 'shrooms.tyva.netherweb.com/~netherweb/signup'
here = 'signup.cgi'
lhere = 'http://%s/%s' % (dompath,here)
shere = 'http://%s/%s' % (dompath,here)

##
## invoice code
##

execfile('invoice.py')

##
## Session code
##

class Session:
	def __init__(self, dbh, form):
		self.dbh = dbh
		self.curmod = ''
		self.sid = form.getfirst('ses','*')
		data = self.checkrow(self.sid)
		if data is None:
			r = random.Random()
			self.sid = ''.join([r.choice('abcdefghjiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ') for x in range(0,32)])
			while self.checkrow(self.sid):
				self.sid = ''.join([r.choice('abcdefghjiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ') for x in range(0,32)])
			cursor = self.dbh.cursor()
			cursor.execute('insert into newac (session_id) values (%s);', self.sid);
			self.token,self.confirmed = '',0
			self.contact,self.refuser,self.planinfo,self.payment,self.acc = {},{},{},{},{}
			expand['fmh1'] = """
<!-- FMH Tracking CODE 1 -->
<!-- Place on Home Page -->
<!-- And all plan sign-up pages -->
<script language="JavaScript">
var qufmh = '' + this.location; 
qufmh = qufmh.substring((qufmh.indexOf('?cif')) + 4);
var keyfmh = qufmh.substring(0,qufmh.indexOf('=')); 
var cifmh = qufmh.substring((qufmh.indexOf('=')) + 1);
if (keyfmh == 'mh') {
  var today = new Date();
  var expiry = new Date(today.getTime() + 2592000000);
  document.cookie="cifmh=" + cifmh + "; expires=" + expiry.toGMTString() + "; domain=netherweb.com; path=/";
}
</script>
<!-- End of Script -->
			"""
		else:
			(self.token,self.confirmed,scontact,srefuser,splaninfo,spayment,sacc) = data
			self.contact = pickle.loads(scontact)
			self.refuser = pickle.loads(srefuser)
			self.planinfo = pickle.loads(splaninfo)
			self.payment = pickle.loads(spayment)
			self.acc = pickle.loads(sacc)
	
	def checkrow(self, sid):
		tmp = None
		cursor = self.dbh.cursor()
		cursor.execute('select token,confirmed,contact,refuser,planinfo,payment,acc from newac where session_id=%s;',self.sid)
		if cursor.rowcount>0:
			tmp = cursor.fetchone()
		cursor.close()
		return tmp
	
	def save(self):
		cursor = self.dbh.cursor()
		cursor.execute('update newac set token=%s,confirmed=%s,contact=%s,refuser=%s,planinfo=%s,payment=%s,acc=%s,last=NOW() where session_id=%s;',
			(self.token,
			self.confirmed,
			pickle.dumps(self.contact),
			pickle.dumps(self.refuser),
			pickle.dumps(self.planinfo),
			pickle.dumps(self.payment),
			pickle.dumps(self.acc),
			self.sid))
		cursor.close()

	def get_token(self):
		r = random.Random()
		if not self.token:
			self.token = ''.join([r.choice('abcdefghjiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ') for x in range(0,32)])
		return self.token
	
	def print_debug(self):
		return ''
		r = ''
		r = r + 'sid = %s\n' % self.sid
		for d in [self.contact,self.refuser,self.planinfo,self.payment,self.acc]:
			r = r + '\n'.join([' * %s => %s' % (k,v) for k,v in d.items()]) + '\n\n'
		return '<pre>' + r + '</pre>'

##
## signup modules
##

class SignupMod:
	def __init__(self, ses, form, cookies, company):
		self.ses = ses
		self.form = form
		self.cookies = cookies
		self.company = company
		self.title = ''
	
	def finished(self):
		return 0

	def ready(self):
		return 0
	
	def do(self):
		self.ses.curmod = self.name
		expand['title'] = self.title
		if self.ready():
			return self.funcs.get(self.form.getfirst('go','default'),self.funcs['default'])()
		return next(self.ses,self.form,self.cookies,self.company)

##
## contact information
##

class Contact(SignupMod): 
	def __init__(self, ses, form, cookies, company):
		SignupMod.__init__(self, ses, form, cookies, company)
		self.name = 'contact'
		self.funcs = {'default': self.email_form,
			'store_email': self.store_email,
			'store_contact': self.store_contact }
		self.title = 'Contact information'

	def finished(self):
		return self.ses.contact.get('finished',0)
	
	def ready(self):
		return 1
	
	def email_form(self, err=''):
		email = self.ses.contact.get('email','')
		r = form_start(self.ses, self.form, {'go': 'store_email'}) + \
			header(strings['CT_EMAIL']) + \
			plain_text(strings['CT_EMAIL1'])
		if err:
			r = r + error(err)
		r = r + \
			'E-mail: ' + ("<input type='text' name='email' value='%s' />" % email) + \
			form_end('OK') + \
			plain_text(strings['CT_PRIVACY'])
		return r
	
	def store_email(self):
		email = self.form.getfirst('email','').strip()
		if not re.match(r'^[0-9a-zA-Z\.\_\-]+@[0-9a-zA-Z\.\_\-]+\.[0-9a-zA-Z\_\-]+$',email):
			return self.email_form("'%s' is not a valid e-mail address." % email)
		self.ses.contact['email'] = email
		self.ses.contact['finished'] = 0
		try:
			cust = self.company.getCustomer(email=email)
			if self.ses.contact.get('new',{}).get(email,0)==0:
				self.ses.contact['previous'] = 1
				self.ses.contact['finished'] = 1
			# make sure the contact has first name, last name, address, and phone
			err = 0
			if not re.search(r'\S',cust.first): err = 1
			if not re.search(r'\S',cust.last): err = 1
			if not re.search(r'\S',cust.address1): err = 1
			if not re.search(r'\S',cust.city): err = 1
			if not re.search(r'\d',cust.dayphone) and not re.search(r'\d',cust.eveningphone): err = 1
			if err:
				if not self.ses.contact.has_key('new'):
					self.ses.contact['new'] = {}
				self.ses.contact['new'][email] = 1
				self.ses.contact['previous'] = 0
				self.ses.contact['finished'] = 0
		except:
			cust = self.company.createCustomer(email=email)
			if not self.ses.contact.has_key('new'):
				self.ses.contact['new'] = {}
			self.ses.contact['new'][email] = 1
			self.ses.contact['previous'] = 0
		if self.ses.contact.get('new',{}).get(email,0)==1:
			return self.contact_form()
		return next(self.ses,self.form,self.cookies,self.company)
	
	def contact_form(self, err=''):
		form_items = [
			('First name', 'first'),
			('Last name', 'last'),
			('Daytime phone', 'dayphone'),
			('Evening phone', 'evephone'),
			('Address (line 1)', 'address1'),
			('Address (line 2)', 'address2'),
			('City', 'city'),
			('State/Province', 'state'),
			('Zip/Postal code', 'zip'),
			('Country', 'country') ]
		table_rows = [(x, '<input type="text" name="%s" value="%s" />' % (y,self.form.getfirst(y,''))) for x,y in form_items]
		return header('Please provide your contact information') + \
			error(err) + \
			form_start(self.ses, self.form, {'go': 'store_contact'}) + \
			table([],table_rows) + \
			form_end('Continue')
	
	def store_contact(self):
		err = ''
		first = self.form.getfirst('first','')
		last = self.form.getfirst('last','')
		dayphone = self.form.getfirst('dayphone','')
		evephone = self.form.getfirst('evephone','')
		addr1 = self.form.getfirst('address1','')
		addr2 = self.form.getfirst('address2','')
		city = self.form.getfirst('city','')
		state = self.form.getfirst('state','')
		zip = self.form.getfirst('zip','')
		country = self.form.getfirst('country','')
		if not re.search(r'\S',first):
			err = err + 'Please provide your first name.<br />'
		if not re.search(r'\S',last):
			err = err + 'Please provide your last name.<br />'
		if not re.search(r'\S',addr1):
			err = err + 'Please provide your address.<br />'
		if not re.search(r'\S',city):
			err = err + 'Please provide your city.<br />'
		if not re.search(r'\S',country):
			err = err + 'Please provide your country.<br />'
		if not re.search(r'\d',evephone) and not re.search(r'\d',dayphone):
			err = err + 'Please provide a daytime or evening phone number.<br />'
		if err:
			return self.contact_form(err)
		cust = self.company.getCustomer(email=self.ses.contact['email'])
		cust.setName(first,last)
		cust.setAddress(addr1,addr2,city,state,zip,country)
		cust.setPhone(dayphone,evephone)
		self.ses.contact['finished'] = 1
		return next(self.ses,self.form,self.cookies,self.company)

##
## refuser/coupon codes
##

class Refuser(SignupMod): 
	def __init__(self, ses, form, cookies, company):
		SignupMod.__init__(self, ses, form, cookies, company)
		self.name = 'refuser'
		self.funcs = {'default': self.ref_form, 'store_ref': self.store_ref, 'done': self.done}
		self.title = 'Referral information'

	def finished(self):
		return self.ses.refuser.get('finished',0)
	
	def ready(self):
		return self.ses.contact.get('finished',0)
	
	def ref_form(self, errc='', erra=''):
		"""Display the form for entering an affiliate/coupon code.
		
		errc -- Error about coupon codes
		erra -- Error about affiliate
		"""
		showref,showcoup = 1,1
		setc = 0
		if self.ses.refuser.get('first',1)==1:
			if self.cookies.get('NWREFID','*')!='*':
				hit_id = self.cookies['NWREFID'].value
				# todo move this out
				cursor = self.ses.dbh.cursor()
				cursor.execute( 'select id,campaign_id from hit where id=%s;', hit_id )
				if cursor.rowcount > 0:
					(hitid,camp_id) = cursor.fetchone()
					cursor.execute('select id from refuser where campaign_id=%s;', camp_id)
					(refid,) = cursor.fetchone()
					self.ses.refuser['id'] = refid
					showref = 0
				cursor.close()
			if self.cookies.get('coupon','*')!='*':
				self.ses.refuser['coupon'] = self.cookies['coupon'].value
		self.ses.refuser['first'] = 0
		r = ''
		r += form_start(self.ses, self.form, {'go': 'store_ref'})
		r += plain_text(strings['REF_COUPON']) + \
			error(errc) + \
			'Coupon code: <input type="text" name="coupon" value="%s" /><br />' % self.ses.refuser.get('coupon','')
		if showref:
			r += plain_text(strings['REF_AFFL']) + \
			error(erra) + \
			'Affiliate username: <input type="text" name="affiliate" /><br />'
		r += form_end('Continue')
		if not showref and not showcoup:
			self.ses.refuser['finished'] = 1
			if setc:
				return self.code_details()
			return next(self.ses,self.form,self.cookies,self.company)
		return r
	
	def store_ref(self, already=1):
		"""Check and store the affiliate/coupon code."""
		errc,erra = '',''
		coupon = self.form.getfirst('coupon','').lower().strip()
		affiliate = self.form.getfirst('affiliate','')
		if coupon:
			if not npricer.checkPricer(coupon,self.ses.dbh):
				errc = "'%s' is not a valid coupon code.  Please double-check your code or leave it blank." % coupon
		if affiliate:
			pass
		if errc or erra:
			return self.ref_form(errc,erra) 
		self.ses.refuser['coupon'] = coupon
		self.ses.refuser['finished'] = 1;
		return self.code_details()
	
	def code_details(self):
		p = npricer.getPricer(self.ses.refuser.get('coupon','default'))(dbh=self.company.dbh)
		if p.describe():
			return plain_text(p.describe()) + \
			form_start(self.ses, self.form, {'mod': 'refuser', 'go': 'done'}) + \
			form_end('Continue')
		return next(self.ses,self.form,self.cookies,self.company)
	
	def done(self):
		return next(self.ses,self.form,self.cookies,self.company)

##
## domain stuff
##

class Domain(SignupMod): 
	def __init__(self, ses, form, cookies, company):
		SignupMod.__init__(self, ses, form, cookies, company)
		self.name = 'domain'
		self.funcs = {
			'default': self.domain_form, 
			'store_domain': self.store_domain, 
			'store_sub': self.store_sub,
			'pwform': self.password_form,
			'store_pw': self.store_password }
		self.title = 'Domain name'

	def finished(self):
		return self.ses.acc.get('finished',0)
	
	def ready(self):
		return self.ses.refuser.get('finished',0)
	
	def domain_form(self, errd='', errs=''):
		domain = self.form.getfirst('domain','')
		subdomain = self.form.getfirst('subdomain','.netherweb.com')
		olddomain = self.ses.acc.get('domain','')
		if (not domain) and (subdomain=='.netherweb.com') and olddomain:
			if re.search(r'\.netherweb\.com$', olddomain):
				subdomain = olddomain
			else:
				domain = olddomain
		return plain_text(strings['ACC_DOMAIN']) + \
			header('Your own domain name: (e.g., yourname.com)') + \
			form_start(self.ses, self.form, {'go': 'store_domain'}) + \
			error(errd) + \
			'Your domain name (e.g., yourname.com) <input type="text" name="domain" value="%s" /><br />' % domain + \
			form_end('Continue') + \
			header('Netherweb Subdomain: (e.g., yourname.netherweb.com)') + \
			form_start(self.ses, self.form, {'go': 'store_sub'}) + \
			plain_text(strings['ACC_SUBDOMAIN']) + \
			error(errs) + \
			'Your subdomain (e.g., yourname.netherweb.com) <input type="text" name="subdomain" value="%s" /><br />' % subdomain + \
			form_end('Continue')
	
	def store_domain(self):
		"""Check and store a domain."""
		error = ''
		domain = self.form.getfirst('domain','').lower().strip()
		m = re.match(r'^www\.(.*)',domain)
		if m:
			domain = m.group(1)
		if not re.match(r'^[a-z0-9\-\.]+\.[a-z]+$',domain):
			error = "'%s' is not a valid domain name." % domain
		if not domain:
			error = 'Please provide a domain name.'
		if not error and self.company.findDomain(domain) is not None:
			error = "The domain '%s' is not available." % domain
		if error:
			return self.domain_form(errd=error)
		self.ses.acc['domain'] = domain
		return self.password_form()
	
	def store_sub(self):
		"""Check and store a subdomain."""
		error = ''
		subdomain = self.form.getfirst('subdomain','').lower().strip()
		if not re.search(r'\.netherweb\.com$',subdomain):
			subdomain = subdomain + '.netherweb.com'
		if not re.match(r'^[a-z0-9\-]+\.netherweb\.com$',subdomain):
			error = "'%s' is not a valid subdomain." % subdomain
		if not error and self.company.findDomain(subdomain) is not None:
			error = "The subdomain '%s' is not available." % subdomain
		if error:
			return self.domain_form(errs=error)
		self.ses.acc['domain'] = subdomain
		return self.password_form()
	
	def password_form(self, err=''):
		"""Display the password form."""
		return header('Please choose a password for your account.') + \
			plain_text(strings['ACC_PASSWORD']) + \
			form_start(self.ses, self.form, {'go': 'store_pw'}) + \
			error(err) + \
			table([],[('Password','<input type="password" name="pw1" value="" />'),
				('Confirm password','<input type="password" name="pw2" value="" />')]) + \
			form_end('Continue') 
	
	def store_password(self):
		pw1 = self.form.getfirst('pw1','')
		pw2 = self.form.getfirst('pw2','')
		if pw1 != pw2:
			return self.password_form('The passwords do not match.  Please re-enter them.')
		if not pw1:
			return self.password_form('Please choose a password.')
		self.ses.acc['password'] = pw1
		self.ses.acc['finished'] = 1
		return next(self.ses,self.form,self.cookies,self.company)

class Plan(SignupMod): 
	def __init__(self, ses, form, cookies, company):
		SignupMod.__init__(self, ses, form, cookies, company)
		self.name = 'plan'
		self.funcs = {'default': self.plan_form, 'store_plan': self.store_plan, 'custom_plan': self.custom_plan, 'store_custom': self.store_custom}
		self.title = 'Hosting plan'

	def finished(self):
		return self.ses.planinfo.get('finished',0)
	
	def ready(self):
		return self.ses.acc.get('finished',0)
	
	def plan_details(self,id):
		r = ''
		shortname,fullname,space,pop3,alias,bw,ptr,stores,cgi,db = self.company.getPlan(prodid=id) 
		if space>0: r = r + '<li>%d MB disk space</li>' % space
		if pop3>0: r = r + '<li>%d POP3 e-mail addresses</li>' % pop3
		if alias>0: r = r + '<li>%d e-mail aliases</li>' % alias
		if bw>0: r = r + '<li>%d GB monthly transfer</li>' % bw
		if stores>0: r = r + '<li>%d integrated storefronts</li>' % stores
		if cgi>0: r = r + '<li>Full CGI/PHP support</li>'
		if db>0: r = r + '<li>%d MySQL databases</li>' % db
		return r
	
	def plan_form(self):
		if self.ses.acc.get('pre',0)==1:
			self.ses.acc['pre'] = 0
			return self.custom_plan()

		# get the pricer and list the available plans
		p = npricer.getPricer(self.ses.refuser.get('coupon','default'))(dbh=self.company.dbh)
		# shortname,fullname,disk space,pop3,alias,bw,ptr,stores,cgi,db
		r = header('Choose your base hosting plan.') + \
			plain_text('You may customize your plan later.')
		for id,shortname,fullname,space,pop3,alias,bw,ptr,stores,cgi,db in [(id,) + self.company.getPlan(prodid=id) for id in p.available()]:
			r = r + '<h3><a href="%s">%s</a></h3>' % (url(self.ses,self.form,{'go':'store_plan','plan':str(id)}),fullname)
			r = r + '<ul>' + self.plan_details(id) + '<li>$%0.02f per month or $%.02f per year</li>' % (p.costOf(id,1,1),p.costOf(id,1,12)-p.discountFor(id,1,12)) + '</ul>' 
		return r
	
	def store_plan(self):
		plan = self.form.getfirst('plan','0')
		self.ses.acc['plan'] = int(plan)
		return self.custom_plan()
	
	def custom_plan(self,err=''):
		plan = self.ses.acc['plan']
		p = npricer.getPricer(self.ses.refuser.get('coupon','default'))(dbh=self.company.dbh)
		cost = p.costOf(self.ses.acc['plan'],1,1) + \
			p.costOf(7,int(self.form.getfirst('space',0)),1) + \
			p.costOf(8,int(self.form.getfirst('pop3',0)),1) + \
			p.costOf(9,int(self.form.getfirst('alias',0)),1) + \
			p.costOf(12,int(self.form.getfirst('xfer',0)),1)
		acost = p.costOf(self.ses.acc['plan'],1,12) - p.discountFor(self.ses.acc['plan'],1,12) + \
			p.costOf(7,int(self.form.getfirst('space',0)),12) - p.discountFor(7,int(self.form.getfirst('space',0)),12) + \
			p.costOf(8,int(self.form.getfirst('pop3',0)),12) - p.discountFor(8,int(self.form.getfirst('pop3',0)),12) + \
			p.costOf(9,int(self.form.getfirst('alias',0)),12) - p.discountFor(9,int(self.form.getfirst('alias',0)),12) + \
			p.costOf(12,int(self.form.getfirst('xfer',0)),12) - p.discountFor(12,int(self.form.getfirst('xfer',0)),12)

		form_items = [('Extra disk space (MB)','space'),('Extra POP3 addresses','pop3'),('Extra e-mail aliases','alias'),('Extra monthly transfer (GB)','xfer')]
		table_rows = [(x, '<input type="text" name="%s" value="%s" />' % (y,self.form.getfirst(y,'0'))) for x,y in form_items]
		return header('Customize your plan') + plain_text(strings['PLAN_CUSTOM']) + \
			plain_text('Base plan: %s' % self.company.getPlan(prodid=plan)[1]) + '<ul>%s</ul>' % self.plan_details(plan) + \
			error(err) + \
			form_start(self.ses, self.form, {'go': 'store_custom'}) + \
			table([],table_rows) + \
			header('Total cost: $%0.02f per month or $%0.02f per year' % (cost,acost)) + \
			'<input type="submit" name="button" value="Recalculate" /><br /><input type="submit" name="button" value="Accept" /><br /></form>'
	def store_custom(self):
		p = npricer.getPricer(self.ses.refuser.get('coupon','default'))(dbh=self.company.dbh)
		if self.form.getfirst('button','Accept')=='Recalculate':
			return self.custom_plan()
		for k in ['space','pop3','alias','xfer']:
			self.ses.acc[k] = int(self.form.getfirst(k,'0'))
		mocost = p.costOf(self.ses.acc['plan'],1,1) + \
			p.costOf(7,self.ses.acc['space'],1) + \
			p.costOf(8,self.ses.acc['pop3'],1) + \
			p.costOf(9,self.ses.acc['alias'],1) + \
			p.costOf(12,self.ses.acc['xfer'],1)
		self.ses.planinfo['mocost'] = mocost
		self.ses.planinfo['anncost'] = p.costOf(self.ses.acc['plan'],1,12) + \
			p.costOf(7,self.ses.acc['space'],12) + \
			p.costOf(8,self.ses.acc['pop3'],12) + \
			p.costOf(9,self.ses.acc['alias'],12) + \
			p.costOf(12,self.ses.acc['xfer'],12) - \
			p.discountFor(self.ses.acc['plan'],1,12) - \
			p.discountFor(7,self.ses.acc['space'],12) - \
			p.discountFor(8,self.ses.acc['pop3'],12) - \
			p.discountFor(9,self.ses.acc['alias'],12) - \
			p.discountFor(12,self.ses.acc['xfer'],12)
		self.ses.planinfo['finished'] = 1
		return next(self.ses,self.form,self.cookies,self.company)

class Payment(SignupMod): 
	def __init__(self, ses, form, cookies, company):
		SignupMod.__init__(self, ses, form, cookies, company)
		self.name = 'payment'
		self.funcs = {'default': self.when_form, 'pay': self.store_when,
			'method_check': self.method_check, 'method_paypal': self.method_paypal, 'method_wire': self.method_wire,
			'method_cc': self.method_cc, 'store_cc': self.store_cc}
		self.title = 'Payment information'

	def finished(self):
		return self.ses.payment.get('finished',0)
	
	def ready(self):
		return self.ses.planinfo.get('finished',0)
	
	def next(self):
		self.ses.payment['finished'] = 1
		return next(self.ses,self.form,self.cookies,self.company)
	
	def when_form(self):
		return plain_text('Please specify whether you wish to pay annually or monthly.') + \
			'<ul><li><a href="%s">Annual payment</a>' % url(self.ses,self.form,{'go':'pay','when':'annual'}) + \
			plain_text('The annual fee for your plan is $%0.02f, a discount of $%0.02f compared with monthly payment.' % (self.ses.planinfo['anncost'],12*self.ses.planinfo['mocost']-self.ses.planinfo['anncost'])) + \
			'</li><li><a href="%s">Monthly payment</a>' % url(self.ses,self.form,{'go':'pay','when':'monthly'}) + \
			plain_text('The monthly fee for your plan is $%0.02f.' % self.ses.planinfo['mocost']) + \
			'</li></ul>'
	
	def store_when(self):
		when = self.form.getfirst('when','monthly')
		if when!='annual':
			when = 'monthly'

		self.ses.payment['when'] = when
		if when=='annual':
			self.ses.planinfo['cost'] = self.ses.planinfo['anncost']
		else:
			self.ses.planinfo['cost'] = self.ses.planinfo['mocost']
		return self.method_form()
	
	def method_form(self):
		r = plain_text('Please select your desired method of payment:') + '<ul>'
		for method in ['cc','check','paypal','wire']:
			r = r + '<li><a href="%s">%s</a>' % (url(self.ses,self.form,{'go':'method_%s'%method},secure=1),strings['PAY_H_%s'%method]) + plain_text(strings['PAY_%s'%method]) + '</li>'
		r = r + '</ul>'
		return r

	def method_check(self):
		self.ses.payment['method'] = 'check'
		return self.next()

	def method_paypal(self):
		self.ses.payment['method'] = 'paypal'
		return self.next()

	def method_wire(self):
		self.ses.payment['method'] = 'wire'
		return self.next()
	
	def method_cc(self, err=''):
		self.ses.payment['method'] = 'cc'
		return plain_text('Please enter your credit card information.') + \
			error(err) + \
			form_start(self.ses,self.form,{'go': 'store_cc'},method='POST',secure=1) + \
			table([],[('Card type:','<select name="cctype"><option value="visa">VISA</option><option value="master">Mastercard</option><option value="discover">Discover</option></select>'),
				('Card number:','<input type="text" name="ccnr" value="" size="19" />'),
				('Expiration date (mm/yy):','<input type="text" name="ccmo" value="" size="2" />/<input type="text" name="ccyr" value="" size="2" />'),
				('Name on card:','<input type="text" name="ccname" value="" size="36" />')]) + \
			plain_text('<input type="checkbox" name="auto" checked /> Automatically pay for hosting each month') + \
			plain_text(strings['PAY_CC_AUTO']) + \
			form_end('Continue')
		return ''
	
	def store_cc(self):
		ccnr = re.sub('\s','',self.form.getfirst('ccnr',''))
		ccmo, ccyr = int(self.form.getfirst('ccmo','0')),int(self.form.getfirst('ccyr','0'))
		ccname = self.form.getfirst('ccname','')
		year, month = time.localtime()[0:2]
		ccerr = ''
		if len(ccnr)!=16:
			ccerr = ccerr + strings['ERR_BILL_CCNR'] + '<br>'
		if not re.search('\s',ccname) or not re.search('\S',ccname):
			ccerr = ccerr + strings['ERR_BILL_NAME'] + '<br>'
		if ccmo < 1 or 12 < ccmo:
			ccerr = ccerr + strings['ERR_BILL_MONTH'] + '<br>'
		if ccyr > 90 or ((2000+ccyr)*12 + ccmo < year*12 + month):
			ccerr = ccerr + strings['ERR_BILL_EXPIRED'] + '<br>'
		if ccerr!='':
			return self.method_cc(err=ccerr)
		if self.form.getfirst('auto','off')=='off':
			self.ses.payment['auto'] = 0
		else:
			self.ses.payment['auto'] = 3
		# todo: Do not set the CC info until after confirming the e-mail address.
		self.ses.payment['cdata'] = nethernet.rsa_encrypt('|'.join([str(ccnr),str(ccname),str(ccmo),str(ccyr)]))
		return self.next()

##
## confirmation, invoice
##

class Confirm(SignupMod): 
	def __init__(self, ses, form, cookies, company):
		SignupMod.__init__(self, ses, form, cookies, company)
		self.name = 'confirm'
		self.funcs = {'default': self.confirm_page, 'finish_it': self.finish_it, 'activate': self.activate, 'paypalipn': self.paypalipn}
		self.title = 'Confirmation'

	def finished(self):
		return 0
	
	def ready(self):
		return self.ses.contact.get('finished',0) and self.ses.refuser.get('finished',0) and self.ses.planinfo.get('finished',0) and self.ses.acc.get('finished',0) and self.ses.payment.get('finished',0)
	
	def make_invoice(self):
		p = npricer.getPricer(self.ses.refuser.get('coupon','default'))(dbh=self.company.dbh)
		months = 'zero Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec'.split()
		year, month, day = time.localtime()[0:3]
		days = nethernet.days_in_month(month, year)
		dur = float(days-day+1)/days
		nextmonth, nextyear = month + 1, year
		if nextmonth>12:
			nextyear = nextyear + 1
			nextmonth = 1

		self.ses.payment['items'] = []
		stuff = [(self.ses.acc['plan'],1)]
		if self.ses.acc['space']>0: stuff.append((7,self.ses.acc['space']))
		if self.ses.acc['pop3']>0: stuff.append((8,self.ses.acc['pop3']))
		if self.ses.acc['alias']>0: stuff.append((9,self.ses.acc['alias']))
		if self.ses.acc['xfer']>0: stuff.append((12,self.ses.acc['xfer']))

		# setup, prorated first month
		self.ses.payment['items'].append((21,1,'Setup fee',p.costOf(21,1,1)))
		self.ses.payment['finalcost'] = p.costOf(21,1,1)
		self.ses.payment['finaldisc'] = 0
		for prod_id,qty in stuff:
			self.ses.payment['items'].append((prod_id,qty,
				'%s: %s %04d (pro-rated)' % (self.company.getProduct(id=prod_id).longname,months[month],year),
				p.costOf(prod_id,qty,dur,0)))
			self.ses.payment['finalcost'] += p.costOf(prod_id,qty,dur,0)
		if self.ses.payment['when']=='annual':
			# next year
			for prod_id,qty in stuff:
				self.ses.payment['items'].append((prod_id,qty,
					'%s: 12 months' % self.company.getProduct(id=prod_id).longname,
					p.costOf(prod_id,qty,12,1)-p.discountFor(prod_id,qty,12,1)))
				self.ses.payment['finalcost'] += p.costOf(prod_id,qty,12,1)-p.discountFor(prod_id,qty,12,1)
				self.ses.payment['finaldisc'] += p.discountFor(prod_id,qty,12,1)
		else:
			# next month, last month
			for prod_id,qty in stuff:
				self.ses.payment['items'].append((prod_id,qty,
					'%s: %s %04d' % (self.company.getProduct(id=prod_id).longname,months[nextmonth],nextyear),
					p.costOf(prod_id,qty,1,1)))
				self.ses.payment['finalcost'] += p.costOf(prod_id,qty,1,1)
				self.ses.payment['items'].append((33,1,
					'%s: Prepayment for final month' % self.company.getProduct(id=prod_id).longname,
					p.costOf(prod_id,qty,1)))
				self.ses.payment['finalcost'] += p.costOf(prod_id,qty,1)
	
	def next(self):
		return next(self.ses,self.form,self.cookies,self.company)
	
	def plan_details(self):
		r = ''
		shortname,fullname,space,pop3,alias,bw,ptr,stores,cgi,db = self.company.getPlan(prodid=self.ses.acc['plan']) 
		space += self.ses.acc['space']
		pop3 += self.ses.acc['pop3']
		alias += self.ses.acc['alias']
		bw += self.ses.acc['xfer']
		if space>0: r = r + '<li>%d MB disk space</li>' % space
		if pop3>0: r = r + '<li>%d POP3 e-mail addresses</li>' % pop3
		if alias>0: r = r + '<li>%d e-mail aliases</li>' % alias
		if bw>0: r = r + '<li>%d GB monthly transfer</li>' % bw
		if stores>0: r = r + '<li>%d integrated storefronts</li>' % stores
		if cgi>0: r = r + '<li>Full CGI/PHP support</li>'
		if db>0: r = r + '<li>%d MySQL databases</li>' % db
		return '<ul>%s</ul>' % r
	
	def confirm_page(self,err=''):
		r = ''
		self.make_invoice()
		r = r + header('Please confirm your order') + error(err) + plain_text('Please check your order information. You may go back and correct anything you wish to change.') + '<ul>'

		cust = self.company.getCustomer(email=self.ses.contact['email'])
		r = r + '<li><a href="%s">Contact information</a>' % url(self.ses,self.form,{'mod':'contact','go':'default'}) + \
			plain_text('%s %s' % (cust.first,cust.last) + '<br />' + cust.email) + '</li>'

		r = r + '<li><a href="%s">Account information</a>' % url(self.ses,self.form,{'mod':'domain','go':'default'}) + \
			plain_text('Domain: %s' % self.ses.acc['domain']) + '</li>'

		coupon = ''
		if self.ses.refuser.get('coupon','default')!='default' and self.ses.refuser.get('coupon','default')!='':
			coupon = plain_text("Using coupon '%s'" % self.ses.refuser.get('coupon','default')) 
		r = r + '<li><a href="%s">Hosting plan</a>' % url(self.ses,self.form,{'mod':'plan','go':'default'}) + \
			coupon + \
			plain_text('Base plan: %s' % self.company.getProduct(id=self.ses.acc['plan']).longname.split(': ')[-1]) + \
			self.plan_details() + '<br /></li>'

		name = {'cc': 'credit card','check': 'check/cash/money order','paypal': 'PayPal','wire': 'wire transfer'}
		dur = 1
		if self.ses.payment['when']=='annual':
			dur = 12
		p = npricer.getPricer(self.ses.refuser.get('coupon','default'))(dbh=self.company.dbh)
		cost = p.costOf(self.ses.acc['plan'],1,dur) - p.discountFor(self.ses.acc['plan'],1,dur) + \
			p.costOf(7,self.ses.acc['space'],dur) - p.discountFor(7,self.ses.acc['space'],dur) + \
			p.costOf(8,self.ses.acc['pop3'],dur) - p.discountFor(8,self.ses.acc['pop3'],dur) + \
			p.costOf(9,self.ses.acc['alias'],dur) - p.discountFor(9,self.ses.acc['alias'],dur)  + \
			p.costOf(12,self.ses.acc['xfer'],dur) - p.discountFor(12,self.ses.acc['xfer'],dur) 
		r = r + '<li><a href="%s">Payment information</a>' % url(self.ses,self.form,{'mod':'payment','go':'default'}) + \
			plain_text('%s payments of $%0.02f' % (self.ses.payment['when'].capitalize(),cost)) + \
			plain_text('Your first invoice will include <ul>%s</ul>' % ''.join(['<li>%s ($%0.02f)</li>' % (desc,cost) for id,qty,desc,cost in self.ses.payment['items']])) + \
			'</li>'

		r = r + '</ul>' + plain_text(strings['CONFIRM_TERMS']) + \
			form_start(self.ses,self.form,{'mod':'confirm','go':'finish_it'}) + \
			plain_text('<input type="checkbox" name="terms" unchecked /> Checking this box indicates that you have read and agree to the hosting terms and conditions.') + \
			plain_text('If this information is correct, you are ready to complete your order.') + \
			form_end('Complete Your Order')
		return r
	
	def finish_it(self):
		if self.form.getfirst('terms','off')=='off':
			return self.confirm_page('Please read the terms and conditions first. Check the box at the bottom of the page to indicate that you have read them.')
		token = self.ses.get_token()
		the_url = url(self.ses,self.form,{'mod': 'confirm', 'go': 'activate', 'token': token}, full=1)
		cust = self.company.getCustomer(email=self.ses.contact['email'])
		cust.sendMail(fromaddr='signup@netherweb.com',
			subject='Please confirm your new Netherweb account.',
			bodytext=strings['CONFIRM_MSG'] % (the_url,the_url,the_url),
			test='')

		return header('Thanks for choosing Netherweb') + \
			plain_text(strings['CONFIRM_EMAIL'])
	
	def check_user(self, user):
		badusers = 'root daemon bin sys sync games man lp news uucp proxy majordom postgres www-data backup msql operator list irc gnats nobody mysql tunl logs control alpha andy ben kevin mattd mattv pierre roland'.split()
		if user in badusers:
			return 0
		cursor = self.company.dbh.cursor()
		cursor.execute('select id from ftpusers where user=%s;',user)
		if cursor.rowcount>0:
			return 0
		cursor.close()
		if len(user)<1:
			return 0
		if user[0].isdigit():
			return 0
		return 1
	
	def get_username(self, domain):
		user = domain.split('.')[0]
		if user.isdigit():
			user = 'u' + user
		user = re.sub(r'^\d+','',user)[0:16]
		count = 1
		while not self.check_user(user):
			user = '%s%02d' % (user[0:14],count)
			count = count + 1
		return user
	
	def activate(self):
		if self.ses.confirmed!=0 and 0:
			return header('Your account has already been confirmed') + plain_text(strings['CONFIRM_ALREADY'])
		expand['fmh2'] = """
<!-- FMH Tracking CODE 2 -->
<!-- Variable Sal = sales$ -->
<!-- 0 can be replaced by $ value of sale -->
<!-- using the variable from your page -->
<!-- but it is not totally essential to do so --> 
<!-- Findmyhosting.com can deduce the sale -->
<!-- From customer actions -->
<script language="JavaScript">
var sal = 0;
var ckie = document.cookie;
var ck1 = ckie.indexOf("cifmh=");
if (ck1 != -1) {
 ck1 = ckie.indexOf("=", ck1) + 1;
 var ck2 = ckie.indexOf(";", ck1);
 if (ck2 == -1){ck2 = ckie.length};
 var cid = ckie.substring(ck1,ck2);
 document.write('<img src="https://www.findmyhosting.com/logsale.asp?cid='+cid+'&hid=6110&val='+sal+'" width="1" height="1">');
}
</script>
<!-- End of Script -->
		"""
		token = self.ses.token
		their_token = self.form.getfirst('token','***')
		if not token or token!=their_token:
			return header('Error activating your account') + plain_text(strings['CONFIRM_ERROR'])
		"""
		# do the newac stuff.
		# pick a server
		r = random.Random()
		server = r.choice([135,136,137,138])
		# create the account
		"""
		cust = self.company.getCustomer(email=self.ses.contact['email'])
		user = self.get_username(self.ses.acc['domain'])
		try:
			vhost = self.company.createVHost(cust,self.ses.acc['domain'],user,self.ses.acc['password'],server)
		except:
			vhost = self.company.getVHost(domain=self.ses.acc['domain'])
		if cust.recv_accid<=0:
			recv = self.company.book.createAccount(str(cust.id) + ' ' + 'receivable','A',0)
			unearn = self.company.book.createAccount(str(cust.id) + ' ' + 'unearned income','L',0)
			# todo: move this
			cursor = self.company.dbh.cursor() 
			cursor.execute('update contacts set recv_accid=%s,unearn_accid=%s where id=%s;',(recv.id,unearn.id,cust.id))
			cust.recv_accid = recv.id
			cust.unearn_accid = unearn.id
		"""
		# log the IP into the history
		vhost.addHist('from IP %s\n' % self.ses.contact['ip_addr'])
		vhost.addHist('coupon code %s\n' % self.ses.refuser['coupon'])

		# set the plan
		vhost.setBasePlan(plan_id=0,product_id=self.ses.acc['plan'])
		vhost.setProductCount(self.company.getProduct(id=7),self.ses.acc['space'])
		vhost.setProductCount(self.company.getProduct(id=8),self.ses.acc['pop3'])
		vhost.setProductCount(self.company.getProduct(id=9),self.ses.acc['alias'])
		vhost.setProductCount(self.company.getProduct(id=12),self.ses.acc['xfer'])

		# set the pricer
		if self.ses.refuser['coupon']!='default':
			vhost.setPricer(self.ses.refuser['coupon'])
		"""
		# sell the items and create the invoice
		year, month, day = time.localtime()[0:3]
		dueyear, duemonth, dueday = year, month, day + 10
		if dueday > nethernet.days_in_month(month,year):
			dueday = dueday - nethernet.days_in_month(month,year)
			duemonth = duemonth + 1
			if duemonth>12:
				dueyear = dueyear + 1
				duemonth = 1
		datestring = '%04d-%02d-%02d' % (year,month,day)
		duedatestring = '%04d-%02d-%02d' % (dueyear,duemonth,dueday)
		# 21 and 33 are setup, so count that revenue now.
		# for the others, wait for monthly billing
		invoice_items = [(self.company.getProduct(id=pid),qty,desc,cost) for pid,qty,desc,cost in self.ses.payment['items']]
		sold_items = [(self.company.getProduct(id=pid),qty,desc,cost) for pid,qty,desc,cost in self.ses.payment['items'] if pid==21 or pid==33]
		invoice = cust.createInvoice(datestring, duedatestring, invoice_items, 0, 0)
		cust.chargeFor(sold_items, invoice.strid)
		#amt = invoice.getTotal()
		amt,disc = self.ses.payment['finalcost'],self.ses.payment['finaldisc']

		tf = tempfile.TemporaryFile()
		tf.write(invoice.exportXML())
		tf.seek(0)

		# charge the CC, if applicable
		paid = 0
		TEST = 0
		declined = 0 
		if self.ses.payment['method']=='cc' and 0:
			#nr,name,mo,yr = cust.getCC('/home/netherweb/secure.prv')
			nr,name,mo,yr = nethernet.rsa_decrypt(self.ses.payment['cdata'],'/home/netherweb/secure.prv').split('|')
			if TEST:
				success,transid,info = 1,'fake',0
			else:
				success,transid,info = ncc.charge_card(self.company.dbh,
					userid=cust.id,name='%s %s' % (cust.first,cust.last),company='',
					address=cust.address1,city=cust.city,state=cust.state,zip=cust.zip,
					country=cust.country,phone=cust.eveningphone,fax=cust.fax,comments='',
					price=amt,item_id=0,item_desc='',
					cc_nr=nr,cc_mo=int(mo),cc_year=int(yr),email=cust.email,
					tr_name='PREAUTH')
			if success:
				cust.recordPayment(method='cc',doc_id=str(transid),amount=float(amt),discount=float(disc))
				paid = 1
				onr,oname,omo,oyear = cust.getCC('/home/netherweb/secure.prv')
				# if there was no old CC data, and the new stuff is valid, remember it
				if onr=='0'*16:
					cust.setCC(nr,name,int(mo),int(yr))
					cust.setMethod('cc')
					cust.setAuto(self.ses.payment.get('auto',3))
			else:
				declined = 1
			
		# create the formatted invoice
		#stuff = tf.read()
		inv = read_invoice(tf)
		if paid:
			inv.pay(str(transid),amt)
			vhost.setNew(2)
		self.ses.payment['invoice_html'] = inv.formatHTML()

		self.ses.confirmed = 1

		pay_us_please = strings['CONFIRM_PAID']
	
	
		# Things to add?
		# image_url -- 150x50 pixel image to use as logo
		# cancel_return -- where the customer goes if he cancels at paypal (paypalcancel)
		# return -- where the customer goes after the payment is concluded
		
		# if we want to add customer info... change _xclick to _ext-enter and add
		# name="redirect_cmd" value="_xclick"
		# There are lots of variables we can use for this...
		# https://www.paypal.com/en_US/pdf/single_item.pdf
		
		if self.ses.payment['method']=='paypal' or 1:
			return header('Your account has been confirmed') + """ 
			Click below to pay via Paypal:
			<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
			<input type="hidden" name="cmd" value="_xclick">
			<input type="hidden" name="business" value="billing@netherweb.com">
			<input type="hidden" name="return" value="%s">
			<input type="hidden" name="currency_code" value="USD"> <input type="hidden" name="item_name" value="Netherweb Hosting: Invoice #%s">
			<input type="hidden" name="invoice" value="%s">
			<input type="hidden" name="amount" value="%.2f">
			<input type="hidden" name="no_shipping" value="1">
			<input type="hidden" name="cancel_return" value="%s">
			<input type="hidden" name="no_note" value="1">
			<input type="hidden" name="notify_url" value="%s">
			<input type="image" src="nwpaypal.png" name="submit" alt="Click here to pay with Paypal">
			</form>""" % (url(self.ses,self.form,{'mod':'paypalconfirm'}),invoice.strid,invoice.strid,amt,url(self.ses,self.form,{'mod':'paypalcancel'}),url(self.ses,self.form,{'go':'paypalipn'},0,1))

		
		return header('Your account has been confirmed') + \
			plain_text(pay_us_please) + \
			plain_text(strings['CONFIRM_ACTIVE'] % url(self.ses,self.form,{'mod':'show_invoice'}))



	def paypalipn(self):
		paymentDetails = sys.stdin.read()
		confirmPostData = paymentDetails + '&cmd="_notify-validate"'
		confirmPostRequest - urllib.urlopen('https://www.paypal.com/cgi-bin/webscr',confirmPostData)
		paymentStatus=confirmPostRequest.read()

		# now paymentStatus has either VERIFIED or INVALID

		# if INVALID, send to support@netherweb.com

		# do stuff with info if VERIFIED

		# check payment_status is Completed
		# check txn_id against previous txn_id
		# check receiver email
		# check mc_gross, mc_currency
		# can check PayPal's digital certificate

		# update database
		# can use item_name / invoice
		
		return header('Does this work?') + """
			j0j0j0j0j0"""

##
## templating routines
##

def display(template):
	print 'Content-type: text/html\n\n'
	print ex(get_template(template))

def display_expand(mo):
	return expand[mo.group(0).strip('!')]

def ex(st,vars=expand):
	return re.sub(r'!(\S+)!',lambda mo: vars[mo.group(0).strip('!')],str(st))

def get_template(filename):
	f = open(templatedir + '/' + filename + '.html', 'r')
	contents = f.read()
	f.close()
	return contents

def plain_text(text):
	return re.sub('\n','<br />','<p>%s</p>' % ('</p><p>'.join(text.split('\n\n'))))

def error(text):
	if text:
		return '<p class="error">%s</p>' % text
	return ''

def header(text):
	return '<h4>%s</h4>' % text

def table(head, rows):
	r = ''
	r = '<table class="form">'
	# skip head for now
	r = r + ''.join(['<tr class="form">%s</tr>' % ''.join(['<td class="form">%s</td>' % x for x in cur]) for cur in rows])
	r = r + '</table>'
	return r

##
## form utilities
##

def form_start(ses, form, args, method='GET', secure=0):
	if 'ses' not in args.keys():
		args['ses'] = ses.sid
	if 'mod' not in args.keys():
		args['mod'] = ses.curmod
	loc = here
	if secure:
		loc = shere
	return ('<form action="%s" method="%s">' % (loc,method)) + '\n'.join(['<input type="hidden" name="%s" value="%s" />' % (k,v) for k,v in args.items()])

def form_end(button):
	return '<input type="submit" value="%s"></form>' % button

def url(ses, form, args, secure=0, full=0):
	if 'ses' not in args.keys():
		args['ses'] = ses.sid
	if 'mod' not in args.keys():
		args['mod'] = ses.curmod
	loc = here
	if full and not secure:
		loc = lhere
	if secure:
		loc = shere
	return loc + '?' + '&'.join(['%s=%s' % (k,v) for k,v in args.items()])

##
## main
##

def links(ses, form, cookies, company):
	r = ''
	for name,mod,obj in [('Contact info','contact',Contact),('Referral info','refuser',Refuser),('Account info','domain',Domain),('Hosting plan','plan',Plan),('Payment info','payment',Payment),('Confirmation','confirm',Confirm)]:
		t = obj(ses, form, cookies, company)
		if ses.curmod==mod:
			r += """<span class="sidelinks"><img src="images/sidelinkwidget.gif" alt="" height="14" width="19" border="0"><font color="white"><b>%s</b></font><br>""" % name
		elif t.ready():
			r += """<span class="sidelinks"><img src="images/sidelinkwidget.gif" alt="" height="14" width="19" border="0"><a href="%s">%s</a><br>""" % (url(ses, form,{'mod': mod}),name)
		else:
			r += """<span class="sidelinks"><img src="images/sidelinkwidget.gif" alt="" height="14" width="19" border="0">%s<br>""" % name
	return r

def next(ses,form,cookies,company):
	for next_mod in [Contact,Refuser,Domain,Plan,Payment,Confirm]:
		do_next = next_mod(ses,form,cookies,company)
		if not do_next.finished():
			break
	else:
		pass
	return do_next.do()

def main():
	form = cgi.FieldStorage()
	cookiestring = ''
	if os.environ.has_key('HTTP_COOKIE'):
		cookiestring = os.environ['HTTP_COOKIE']

	cookies = Cookie.SimpleCookie()
	cookies.load(cookiestring)

	mdbh = MySQLdb.connect(user=os.environ['MAINDBUSER'],
		passwd=os.environ['MAINDBPASSWORD'],
		host=os.environ['MAINDBSERVER'],
		db=os.environ['MAINDBNAME'])
	adbh = MySQLdb.connect(user=os.environ['ACCDBUSER'],
		passwd=os.environ['ACCDBPASSWORD'],
		host=os.environ['MAINDBSERVER'],
		db=os.environ['ACCDBNAME'])

	book = naccount.Book(dbh=adbh)
	company = nethernet.Company(dbh=mdbh,book=book)

	ses = Session(mdbh,form)
	ses.contact['ip_addr'] = os.environ.get('REMOTE_ADDR','WHERE_IS_THE_IP')

	if form.getfirst('shared',0):
		ses.acc['plan'] = form.getfirst('shared')
		ses.acc['pre'] = 1

	modules = {'contact': Contact, 'refuser': Refuser, 'domain': Domain, 'plan': Plan, 'payment': Payment, 'confirm': Confirm}
	if modules.has_key(form.getfirst('mod','none')):
		do_next = modules[form.getfirst('mod')](ses,form,cookies,company)
		expand['content'] = do_next.do()
	elif form.getfirst('mod','none')=='show_invoice' and ses.payment.get('invoice_html','*')!='*':
		print 'Content-type: text/html\n\n'
		print ex(get_template('invoice'),ses.payment['invoice_html'])
		return
	else:
		expand['content'] = next(ses,form,cookies,company)
	
	#expand['content'] += ses.print_debug()

	ses.save()

	expand['sidelinks'] = links(ses, form, cookies, company)
	display('template')

if __name__ == '__main__':
	main()
