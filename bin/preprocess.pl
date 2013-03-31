#!/opt/local/bin/perl
#
# This script processes a source file containing unit tests and provides several
# useful features for simplifying construction and collection of test.
# provides two major conveniences for test developers.
#
# Tests are designated with a separate line of the form
#
# @test
# subroutine <yourtest>()
#
#
#


use File::Basename;

my $fname = $ARGV[0];
open my $infile, '<', $fname or die "Can't open $ARGV[0]";

my $lineNumber = 0;
my @tests;
(my $file, my $dir, my $ext) = fileparse($fname, qr/\.[^.]*/);
my $suiteName = $file;

while ( my $line = <$infile> ) {  # process each line in the source file
    $lineNumber++;
    if ($line =~ s/(\s*)\@assertEqual\((.*)\)/\1call assertEqual(\2, &
    & line=$lineNumber, &
    & file='$fname')/i) {
	print "#line ", $lineNumber, " \"$fname\"" , "\n";
	print $line;
	next;
    }
    if ($line =~ /\@test/) {
        my $nextLine = <$infile>;
	my $testName = $nextLine;
	$testName =~ s/ *subroutine *(.*) *\(.*\)/\1/i;
	push (@tests, $testName);
	print $nextLine;
    }
    else {
	print $line
    }

    
}

print "!--------------------------------------------------\n";
print "! Suite Factory for tests in file $suiteName.\n";
print "!--------------------------------------------------\n";

print "function $suiteName() result(suite)\n";
print "   use pfunit_mod\n";
print "   implicit none\n";
print "   type (TestSuite) :: suite\n";
print " \n";

foreach (@tests) {
    my $test = $_;
    chomp($test);
    print "  external $test \n";
}

print " \n";
print "   suite = newTestSuite('$suiteName')\n";
print " \n";

foreach (@tests) {
    my $test = $_;
    chomp($test);
    print "  call suite%addTest(newTestMethod(\"$test\", $test))\n";
}

print " \n";
print "end function $suiteName\n";



