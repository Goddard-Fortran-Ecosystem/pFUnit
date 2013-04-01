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
my @mpiTests;
(my $file, my $dir, my $ext) = fileparse($fname, qr/\.[^.]*/);
my $suiteName = $file;

while ( my $line = <$infile> ) {  # process each line in the source file
    $lineNumber++;
    if ($line =~ s/(\s*)\@assertEqual\((.*)\)/\1call assertEqual(\2, &
     & line=$lineNumber, &
     & file='$fname')/i) {
	print "#line ", $lineNumber, " \"$fname\"" , "\n";
	print $line;
	print "   if (exceptionWasThrown()) return \n";
	next;
    }
    if ($line =~ /^\@test/) {
        my $nextLine = <$infile>;
	my $testName = $nextLine;
	$testName =~ s/ *subroutine *(.*) *\(.*\)/\1/i;
	push (@tests, {"name" => $testName});
	print $nextLine;
    }
    elsif ($line =~ /^\@mpiTest/) {
	my $npesString = $line;
	$npesString =~ s/.*npes\s*=\s*\[(.*)\]/\1/i;
	
	my @npes = split(',',$npesString);
        my $nextLine = <$infile>;
	my $testName = $nextLine;
	$testName =~ s/ *subroutine *(.*) *\(.*\)/\1/i;
	
	push (@mpiTests, {"name" => $testName, "npes" => \@npes});
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

foreach (@tests,@mpiTests) {
    my $test = $_->{"name"};
    chomp($test);
    print "  external $test \n";
}

print " \n";
print "   suite = newTestSuite('$suiteName')\n";
print " \n";

foreach (@tests) {
    my $test = $_->{"name"};
    chomp($test);
    print "  call suite%addTest(newTestMethod(\"$test\", $test))\n";
}

foreach (@mpiTests) {
    my $test = $_->{"name"};
    chomp($test);
    my @npes = @{$_->{"npes"}};

    foreach (@npes) {
	chomp;
	print "  call suite%addTest(newMpiTestMethod(\"$test\", &
      &    $test, &
      &    numProcesses=$_))\n";
    }
}

print " \n";
print "end function $suiteName\n";



