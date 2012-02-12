#!/usr/bin/perl -w

use strict;
use warnings;
use Net::FTP;

my $host = $ARGV[0];
my $dir = $ARGV[1];
my $get = $ARGV[2];
my $put = $ARGV[3];
my $ip = $ARGV[4];

my %firewall = (
	'192.168.0.1','',
	'127.0.0.1',''
);

$#ARGV == 4 or die "Not enough arguments\n";
exists($firewall{$ip}) or exit 401;

($get =~ /\.txt$/i) or exit 403;

#my $ftp = Net::FTP->new($host, Timeout => 50) or exit 1;
my $ftp;
if(!($ftp = Net::FTP->new($host, Timeout => 50))) {
	`echo FTP site down > $put`;
	exit 0;
}
$ftp->login() or die "Couldn't login\n";
$ftp->cwd($dir) or die "Couldn't change directory\n";
$ftp->binary;
$ftp->get($get, $put) or die "Couldn't get file\n";

$ftp->quit;
