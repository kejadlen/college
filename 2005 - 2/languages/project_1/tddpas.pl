#!/usr/bin/env perl -w
#=============================================================================
#   @(#)$Id$
#-----------------------------------------------------------------------------
#   Test-driven development script for Pascal programs and other
#   executables that take input on stdin and produce output on
#   stdout.  See course web page for details on test case format and
#   usage.
#
#   usage summary:
#       tddpas.pl <executable-file-name> <test-case-file-name> 
#=============================================================================

use strict;
use English;

if ( $#ARGV != 1 )
{
    die "usage:\n    tddpas.pl <executable-file-name> <test-case-file-name>\n";
}

#=============================================================================
# Bring command line args into local variables for easy reference
#=============================================================================

my $student_exec  = $ARGV[0];	# source file name 
my $student_tests = $ARGV[1];	# test case file
                                # (includes expected output)


#=============================================================================
# In addition, some local definitions within this script
#=============================================================================
my $version         = "1.3";
my @labels          = ();	# User-provided test case names
my @test_cases	    = ();	# test case input
my @expected_output = ();	# corresponding expected output
my @case_nos        = ();       # test case number for each output line
my $temp_input      = "$PID.in";  # Name for temp test input file
my $temp_output     = "$PID.out"; # Name for temp test output file
my $delete_temps    = 1;        # Change to 0 to preserve temp files


#=============================================================================
# A subroutine for normalizing output lines before comparing them
#=============================================================================
sub normalize
{
    my $line = shift;
    $line =~ s/^\s+//o;		# Trim leading space
    $line =~ s/\s+$//o;		# Trim trailing space
    $line =~ s/\s+/ /go;	# Convert multi-space sequences to one space
    $line =~ s,\s+/\s+,/,go;    # Remove spaces around /'s
    $line =~ tr/A-Z/a-z/;	# Convert to lower case
    return $line;
}


#=============================================================================
# Phase I: Parse and split the test case input file
#=============================================================================

open ( CASES, $student_tests ) ||
    die "Cannot open '$student_tests': $!";

my $scanning_input = 0;
my $case = -1;
while ( <CASES> )
{
    # skip comment lines
    next if ( m,^//(?!--|==),o );

    if ( m,^//==,o )
    {
	if ( $scanning_input )
        {
	    print "$student_tests: ", $INPUT_LINE_NUMBER - 1,
	          ": improperly formatted test case.\n";
        }
	my $label = $_;
	chomp $label;
	$label =~ s,^//==[-=\s]*,,o;
	$label =~ s,[-=\s]*$,,o;
	# if ( $label eq "" ) { $label = "(no label)"; }
	push( @labels, $label );
	push( @test_cases, "" );
	$case++;
	$scanning_input = 1;
    }
    elsif ( m,^//--,o )
    {
	if ( ! $scanning_input )
	{
	    print "$student_tests: ", $INPUT_LINE_NUMBER,
	        ": improperly formatted test case; cannot proceed.\n";
	}
	$scanning_input = 0;
    }
    else
    {
	if ( $scanning_input )
	{
	    # Then this is an input line
	    if ( $#test_cases < 0 )
	    {
	        print "$student_tests: ", $INPUT_LINE_NUMBER,
	              ": improperly formatted test case.\n";
	    }
	    else
	    {
		$test_cases[$#test_cases] .= $_;
		if ( $labels[$#labels] eq "" )
		{
		    # Use first line of input for case label
		    chomp;
		    $labels[$#labels] = $_;
		}
	    }
	}
	else
	{
	    if ( $#labels < 0 )
	    {
		print "$student_tests: ", $INPUT_LINE_NUMBER,
	              ": improperly formatted test case.\n";
	    }
	    push( @expected_output, normalize( $_ ) );
	    push( @case_nos, $case );
	}
    }
}

## For debugging purposes:
# print "labels => '", join( "', '", @labels ), "'\n";
# print "inputs => '", join( "', '", @test_cases ), "'\n";
# print "outputs => '", join( "', '", @expected_output ), "'\n";
# print "case_nos => '", join( "', '", @case_nos ), "'\n";


#=============================================================================
# Phase II: Execute the program
#=============================================================================

print "\ntddpas.pl v$version   (c) 2004 Virginia Tech. All rights reserved.\n";
print "Testing $student_exec using $student_tests\n";
open( INFILE, "> $temp_input" ) ||
    die "Cannot open '$temp_input': $!";
print INFILE for @test_cases;
close( INFILE );
system( "$student_exec < $temp_input > $PID.out" );
if ( $delete_temps )
{
    unlink( "$temp_input" );
}


#=============================================================================
# Phase III: Compare the output to test case expectations
#=============================================================================

my $line        = 0;    # next line in @expected_output to match
my $last_failed = -1;	# index of last failed case
my $failures    = 0;    # count of failures
my $errs        = 0;    # Number of runtime errors, which is 0 or 1 since
                        # such an error crashes the program

open( STUDENT, "$temp_output" ) ||
    die "Cannot open temporary output file '$temp_output': $!";

while ( <STUDENT> )
{
    if ( $line > $#expected_output )
    {
	# If the expected output has run out, just add up the remaining
	# lines as errors or crashes.
	while ( defined $_ )
	{
	    if ( m/^runtime error/io )
	    {
		$errs++;
		last;
	    }
	    $failures++;
	    $_ = <STUDENT>;
	}
	last;
    }

    # If the line does not match the expected output
    if ( normalize( $_ ) ne $expected_output[$line] )
    {
	if ( $line % 78 == 0 )
	{
	    print "\n";
        }
	my $this_fail = $case_nos[$line];
	print "F";
	if ( $this_fail != $last_failed )
	{
	    print "\ncase ", $this_fail + 1,
	          " FAILED: $labels[$this_fail]\n";
	    $failures++;
	    print "  Expected: '$expected_output[$line]'\n";
	    print "       Got: '", normalize( $_ ), "'\n";
	}
	$last_failed = $this_fail;
    }
    else
    {
	if ( $line % 78 == 0 )
	{
	    print "\n";
        }
	print ".";
    }
    $line++;
}

close( STUDENT );

if ( $line <= $#expected_output )
{
    $failures += $#expected_output - $line + 1;
}

if ( $failures + $errs == 0  &&  $delete_temps  )
{
    unlink( $temp_output );
}

my $num_cases = $#labels + 1;
print "\n\nTests Run: $num_cases, Errors: $errs, Failures: $failures (",
    sprintf( "%.1f", ($num_cases - $failures - $errs)/$num_cases*100 ),
    "%)\n";
if ( $failures + $errs > 0 )
{
    print "Output has been saved in $temp_output.\n";
}

#=============================================================================
# Exit Script
#=============================================================================
exit(0);
