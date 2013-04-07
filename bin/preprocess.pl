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
my $moduleName = "";
my $constructor = "newTestMethod";
my @tests;
my @mpiTests;
(my $file, my $dir, my $ext) = fileparse($fname, qr/\.[^.]*/);
my $suiteName = $file;
my $setUp = "";
my $tearDown = "";

while ( my $line = <$infile> ) {  # process each line in the source file
    $lineNumber++;
    if ($line =~ s/^(\s*)\@assertEqual\((.*)\)/\1call assertEqual(\2, &
     & file='$fname', line=$lineNumber)/i) {
	print "#line ", $lineNumber, " \"$fname\"" , "\n";
	print $line;
	print "   if (anyExceptions()) return \n";
	next;
    }
    elsif ($line =~ /^\s*module\s+procedure/i) { # don't treat as a module
	print "$line";
    }
    elsif ($line =~ /^\s*module\s+/i) {
	$moduleName = $line;
	$moduleName =~ s/\s*module\s+(\w*)$/\1/i;
	chomp($moduleName);
	$suiteName=$moduleName."_suite";
	print "$line";
    }
    elsif ($line =~ /^\@TestCase/i) {
        my $nextLine = <$infile>;
	$lineNumber++;
	$constructor = $nextLine;
	$constructor =~ s/^.*::\s*(\w*)/\1/i;
	$constructor = $constructor;
	chomp($constructor);
	print "!$line";
	print $nextLine;
    }
    elsif ($line =~ /^\@test/i) {
        my $nextLine = <$infile>;
	$lineNumber++;
	my $testName = $nextLine;
	$testName =~ s/ *subroutine *(.*) *\(.*\)/\1/i;
	push (@tests, {"name" => $testName});
	print "!$line";
	print $nextLine;
    }
    elsif ($line =~ /^\@mpiTest/i) {
	my $npesString = $line;
	$npesString =~ s/.*npes\s*=\s*\[(.*)\]/\1/i;
	
	my @npes = split(',',$npesString);
        my $nextLine = <$infile>;
	$lineNumber++;
	my $testName = $nextLine;
	$testName =~ s/ *subroutine *(.*) *\(.*\)/\1/i;
	
	push (@mpiTests, {"name" => $testName, "npes" => \@npes});
	print "!$line";
	print $nextLine;
    }
    elsif ($line =~ /^\@Before/i) {
        my $nextLine = <$infile>;
	$lineNumber++;
	$setUp = $nextLine;
	$setUp =~ s/ *subroutine *(.*) *\(.*\)/\1/i;
	chomp($setUp);
	print "!$line";
	print $nextLine;
    }
    elsif ($line =~ /^\@After/i) {
        my $nextLine = <$infile>;
	$lineNumber++;
	$tearDown = $nextLine;
	$tearDown =~ s/ *subroutine *(.*) *\(.*\)/\1/i;
	chomp($tearDown);
	print "!$line";
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
if ($moduleName ne "") {
    print "   use $moduleName\n";
}
print "   implicit none\n";
print "   type (TestSuite) :: suite\n";
print " \n";

if ($moduleName eq "") {
    foreach (@tests,@mpiTests) {
	my $test = $_->{"name"};
	chomp($test);
	print "   external $test \n";
    }

    if ($setUp ne "") {
	print "   external $setUp \n";
    }
    if ($tearDown ne "") {
	print "   external $tearDown \n";
    }
}
	

print " \n";
print "   suite = newTestSuite('$suiteName')\n";
print " \n";

if ($setUp ne "") {$setUp = ", $setUp";}
if ($tearDown ne "") {$tearDown = ", $tearDown";}

foreach (@tests) {
    my $test = $_->{"name"};
    chomp($test);

    print "  call suite%addTest($constructor(\"$test\", $test $setUp $tearDown))\n";
}

foreach (@mpiTests) {
    my $test = $_->{"name"};
    chomp($test);
    my @npes = @{$_->{"npes"}};

    foreach (@npes) {
	chomp;
	print "  call suite%addTest(newMpiTestMethod(\"$test\", &
      &    $test $setUp $tearDown, &
      &    numProcesses=$_))\n";
    }
}

print " \n";
print "end function $suiteName\n";



