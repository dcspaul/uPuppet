#!/usr/bin/perl

# the JSON stuff is a bit confusing ....
# - JSON::XS is a fast JSON parser but it uses native compiled code which needs to be different
#   on each platform - and you need to download and install it from CPAN
# - JSON::PP is a pure-perl implementation which is compatible with JSON:XS (but slower)
#   this is now included in the Perl core and should hence be available by default
# - JSON should attempt to use JSON::XS if it is present, but will use JSON:PP if not
# - We make an explicit call to JSON:PP methods
#   I'm not actually sure that this is necessary, but ....
#   We explicitly use the JSON:PP so it all works
# see: http://search.cpan.org/~makamaka/JSON-2.90/lib/JSON.pm

use JSON::PP;

=begin comment

run-tests

Paul Anderson <dcspaul@ed.ac.uk>

=cut

use strict;
use Getopt::Std;
use File::Basename;
use Text::Tabs;

my $prog = "run-tests";				# program name

my $verbose = 1;					# verbosity level (0-2)
my $nocomp = 0;						# compile tests locally (uPuppet) only - do not compare
my $recompile = 0;					# recompile cached puppet files
my $testSummary = 0;				# summarize test coverage (1=short,2=long)
my $summaryFile = undef;			# file containing latex for test summary (to be updated)
my $inFile = undef;					# single file to compile
my $latexOpts = "2fme";				# default options to remote puppet_compile for creating latex
my $latexOutput = 0;				# create latex output
my $local = 0;						# run puppet on local machine

my $baseDir = undef;				# base directory for uPuppet
my $testDir = undef;				# Test directory
my $tmpDir = undef;					# Tmp directory
my $binDir = undef;					# Bin directory
my $cacheDir = undef;				# Cache of Puppet output
my $vm = undef;						# the name of the Vagrant subdirectory for the vm
my $vmDir = undef;					# the Vagrant directory

my $fails = 0;						# number of failing tests
my $oks = 0;						# number of succeeding tests
my $featureTable = {};				# count of passes/unsupported/fails per feature

#########################################################################################
sub Usage() {
##########################################################################################

	print stderr <<EOF;
usage: $prog [opts] file ...
 -f               - summarize coverage by feature (short)
 -F               - summarize coverage by feature (long)
 -l               - create latex output
 -L               - run Puppet locally (instead of via vagrant)
 -m <vm>          - the vm to use for native compilations (default "Ubuntu")
 -n               - compile with uPuppet only (no comparison)
 -r               - recompile cached Puppet output
 -S <file>        - current source file
 -t <opts>        - puppet-compile -t options for creating latex output
 -u <file>        - update latex file with coverage summary
 -v               - verbose
EOF
	exit(1);
}

##########################################################################################
sub GetArguments() {
##########################################################################################

	my $opts = {};
	my $opt_string = 'fFLlm:nrS:t:u:v';

	# options
	if (getopts( $opt_string, $opts )) {
		$nocomp=1 if ($opts->{'n'});
		$verbose=2 if ($opts->{'v'});
		$recompile=1 if ($opts->{'r'});
		$latexOutput=1 if ($opts->{'l'});
		$local=1 if ($opts->{'L'});
		$testSummary = ($opts->{'F'} ? 3 : 1) if ($opts->{'f'});
		$testSummary = ($opts->{'f'} ? 3 : 2) if ($opts->{'F'});
		$inFile=$opts->{'S'} if (defined($opts->{'S'}));
		$vm=$opts->{'m'} if (defined($opts->{'m'}));
		$summaryFile=$opts->{'u'} if (defined($opts->{'u'}));
		$latexOpts=$opts->{'t'} if (defined($opts->{'t'}));
	} else { $opts = undef; }

	# sources
	my @sources = @ARGV;

	# incompatible options
	if ( ( $testSummary>0 || defined($summaryFile) ) && $latexOutput ) {
		print stderr "$prog: summary options not compatible with latex generation\n";
		exit(1);
	}

	# defaults
	$verbose = 1 unless (defined($verbose));
	$vm = 'Ubuntu' unless (defined($vm));

	Usage() unless (defined($opts));
	return \@sources;
}

##########################################################################################
sub RunTest($) {
##########################################################################################

	my $srcFile = shift;

	my $tag="** FAIL";

	# output from "real" Puppet goes in the cache directory
	# with a name based on the input, so we can cache it for performance.
	# output from uPuppet is not cached (it is fast enough and we might
	# have changed the compiler) and we use a single name to prevent
	# proliferation of unwanted temporary output files

	my $filename = $srcFile; $filename =~ s/\.pp//;
	my $outFile = "$cacheDir/Puppet/$filename";
	my $outDir = $outFile; $outDir =~ s/\/[^\/]*$//;
	my $uOutFile = "$tmpDir/output";
	my $uOutDir = $uOutFile; $uOutDir =~ s/\/[^\/]*$//;

	my $annotations = FindAnnotations($srcFile);

	# create Latex output
	if ($latexOutput) {
		MakeLatex($srcFile,$annotations);
		return;
	}

	# extract any annotations from the source file
	my $expectFail = defined($annotations->{'EXPECT_FAIL'});
	my $unsupported = defined($annotations->{'UNSUPPORTED'});
	my $unordered = defined($annotations->{'UNORDERED'});
	my $features = $annotations->{'FEATURES'};
	my $pc_opts = $annotations->{'OPTIONS'};

	# Compile using uPuppet
	print stderr "\n" if ($verbose>1);
	print stderr "====> Testing $srcFile ...\n";
	if ($verbose>1) {
		print stderr "\n";
		unless (open(FP,"<$testDir/$srcFile")) {
			print stderr "failed to open: $testDir/$srcFile\n$!\n\n";
			return;
		}
		my $n=0; while (<FP>) { printf stderr "%02d: %s", ++$n, $_; }
	}
	print stderr "\n====> Compiling $srcFile with uPuppet ...\n\n" if ($verbose>1);
	my $result = `mkdir -p $uOutDir 2>&1`;
	unless ($?==0) {
		print stderr "$prog: failed to create output directory: $uOutDir\n";
		print stderr "$prog: $result\n";
		exit(1);
	}
	my $cmd1 = "$binDir/uPuppet -m -o - -f json -s 1000 $testDir/$srcFile";
	my $nothing1 = `$cmd1 >$uOutFile.out 2>$uOutFile.err`;
	my $status1 = $?;
	my $stdout1 =`cat $uOutFile.out 2>/dev/null`; chomp $stdout1;
	my $stderr1 =`cat $uOutFile.err 2>/dev/null`; chomp $stderr1;
	my $text1; if ($status1 == 0) {
		$text1 = eval { decode_json($stdout1); };
		if (defined($text1)) {
			chomp $text1;
			my $json = JSON::PP->new()->pretty([1])->canonical([1]);;
			my $pretty = $json->encode($text1);
			print stderr "COMPILED OK:\n$pretty\n" if ($verbose>1);
		} else {
			my $msg = $@; chomp $msg;
			$msg =~ s/\s+at\s+\S+\s+line\s+\d+\.$//;
			print stderr "INVALID OUTPUT:\n$msg\n" if ($verbose>1 || $nocomp);
			$status1=1;
		}
	} else {
		print stderr "COMPILE FAILED:\n$stderr1\n" if ($verbose>1 || $nocomp);
	}

	# showing local compilations only
	return if ($nocomp);

	# Compile using real Puppet

	# Check the cache
	my $srcTime = (stat("$testDir/$srcFile"))[9];
	my $errTime = (stat("$outFile.err"))[9];
	my $isCached = defined($srcTime) && defined($errTime) && ($errTime>=$srcTime);

	my $status2; if ($recompile || !$isCached) {

		my $omsg = $pc_opts ? " ($pc_opts)" : "";
		print stderr "\n====> Compiling $srcFile on $vm$omsg ...\n\n" if ($verbose>1);
		my $result = `mkdir -p $outDir 2>&1`;
		unless ($?==0) {
			print stderr "$prog: failed to create output directory: $outDir\n";
			print stderr "$prog: $result\n";
			exit(1);
		}
		my $cmd2; if ($local) {
			$cmd2 = "puppet-compile -o '$pc_opts' -- - <$testDir/$srcFile";
		} else {
			$cmd2 = "cd $vmDir; vagrant ssh -- \". /etc/profile; puppet-compile";
			$cmd2 .= " -o '$pc_opts' -- -\" <$testDir/$srcFile";
		}
		my $nothing2 =`$cmd2 >$outFile.out 2>$outFile.err`;
		$status2 = $?;

	} else {

		print stderr "\n====> Using $srcFile result from cache ...\n\n" if ($verbose>1);
		$status2 = ( (-f "$outFile.out") && ((stat "$outFile.out")[7]>0) ) ? 0 : 1;
	}

	my $stdout2 =`cat $outFile.out 2>/dev/null`; chomp $stdout2;
	my $stderr2 =`cat $outFile.err 2>/dev/null`; chomp $stderr2;
	my $text2; if ($status2 == 0) {
		$text2 = eval { decode_json($stdout2); };
		if (defined($text2)) {
			chomp $text2;
			my $json = JSON::PP->new()->pretty([1])->canonical([1]);
			my $pretty = $json->encode($text2);
			print stderr "COMPILED OK:\n$pretty\n" if ($verbose>1);
		} else {
			my $msg = $@; chomp $msg;
			$msg =~ s/\s+at\s+\S+\s+line\s+\d+\.$//;
			print stderr "INVALID OUTPUT:\n$msg\n";
			$status2=1;
		}
	} else {
		unlink("$outFile.out"); # no output file in cache means we had an error
		if ($stderr2 =~ /VM must be running to open SSH connection./) {
			print stderr "$prog: running VM required to compile Puppet source\n";
			print stderr "$prog: try running \"make start-ubuntu\"\n\n";
			unlink("$outFile.err");
			exit(1);
		} else {
			print stderr "COMPILE FAIL:\n$stderr2\n" if ($verbose>1);
		}
	}

	# Report the status

	my $status; if ($status1==0) {
		if ($status2==0) {
			print stderr "\n====> Comparing Catalogs ...\n" if ($verbose>1);
			my $msgs = CompareCatalogs($unordered,$text1,$text2);
			if (!$msgs) {
				$status =  ($expectFail)
					? "both expected to fail, but both compiled ok (catalogs match)"
					: ($unsupported) ? "uPuppet unsupported, but both compiled ok (catalogs match)"
					: "both compiled ok & catalogs match";
				$tag = "OK" unless ($expectFail || $unsupported);
			} else {
				$status =  ($expectFail)
					? "both expected to fail, but both compiled ok (catalogs differ)\n$msgs"
					: ($unsupported) ? "uPuppet unsupported, but both compiled ok (catalogs differ)\n$msgs"
					: "both compiled ok, but catalogs differ\n$msgs";
				chomp $status;
				$tag = "OK" if ($unsupported);
			}
		} else {
			$status = ($expectFail)
				? "both expected to fail, but uPuppet compiled OK (Puppet failed)"
				: ($unsupported) ? "uPuppet unsupported but compiled OK (Puppet failed)"
				: "uPuppet compiled OK, but Puppet failed";
		}
	} else {
		if ($status2==0) {
			$status = ($expectFail)
				? "both expected to fail, but Puppet compiled OK"
				: ($unsupported) ? "uPuppet unsupported and failed as expected (Puppet compiled OK)"
				: "uPuppet failed, but Puppet compiled OK";
			$tag = "OK" if ($unsupported);
		} else {
			$status = ($expectFail)
				? "both failed (as expected)"
				: ($unsupported) ? "uPuppet unsupported and failed as expected, but Puppet failed too"
				: "both failed";
			$tag = "OK" if ($expectFail);
		}
	}

	if ($tag eq 'OK') { ++$oks; } else { ++$fails; }

	print stderr "\n" if ($verbose>1);
	print stderr "$tag: $status\n";
	print stderr "\n" if ($verbose>1);

	# record the files in the feature table by feature 
	my @fs = map { s/^\s+//; s/\s+$//; $_; } ( split /,/, $features );
	@fs = ( "unclassified", "misc" ) unless (scalar @fs > 0);
	foreach my $keyword (@fs) {
		$featureTable->{$keyword} = { PASS=>[], FAIL=>[], UNSUPPORTED=>[] }
			unless (defined($featureTable->{$keyword}));
		my $l;
		if ($tag ne 'OK') { $l=$featureTable->{$keyword}->{'FAIL'}; }	
		elsif ($unsupported) { $l=$featureTable->{$keyword}->{'UNSUPPORTED'}; }
		else { $l=$featureTable->{$keyword}->{'PASS'}; }
		push @$l,$filename;
	}
}

##########################################################################################
sub FindAnnotations($) {
##########################################################################################

	my $srcFile = shift;

	# the source file can be annotated by using comments of the form:
	# # .... @FOO ... @BAR ...
	# these can be used to control aspects of the compilation or comparison
	# Eg. @EXPECT_FAIL marks a source file as expected to fail compilation

	my $annotations = {};
	unless (open FP,"<$testDir/$srcFile") {
		print stderr "$prog: failed to open source file: $testDir/$srcFile\n";
		print stderr "$prog: $!\n";
		exit(1);
	}
	while (<FP>) {
		my $line = $_;
		while ($line =~ /^(#.*)\@([A-Za-z_][A-Za-z0-9_]*)\s*([^\@]*)(.*)$/) {
			my $tag=$2; my $arg=$3; $line = "$1$4";
			$arg =~ s/^\s*//; $arg =~ s/\s*$//;
			if (defined($annotations->{$tag})) { $annotations->{$tag} .= " , $arg"; }
			else { $annotations->{$tag}=$arg; }
		}
	}
	close(FP);
	return $annotations;
}

##########################################################################################
sub MakeLatex($$) {
##########################################################################################

	my $srcFile = shift;
	my $annotations = shift;

	my $pc_opts = $annotations->{'OPTIONS'};
	my $filename = $srcFile; $filename =~ s/\.pp//;
	my $outFile = "$cacheDir/Latex/$filename";
	my $outDir = $outFile; $outDir =~ s/\/[^\/]*$//;

	my $omsg = $pc_opts ? " ($pc_opts)" : "";
	print stderr "\n====> Creating Latex for $srcFile on $vm$omsg ...\n\n" if ($verbose>1);
	my $result = `mkdir -p $outDir 2>&1`;
	unless ($?==0) {
		print stderr "$prog: failed to create output directory: $outDir\n";
		print stderr "$prog: $result\n";
		exit(1);
	}
	my $cmd; if ($local) {
		$cmd = "puppet-compile -t $latexOpts -N \"$srcFile\" -o '$pc_opts' -- - <$testDir/$srcFile";
	} else {
		$cmd = "cd $vmDir; vagrant ssh -- \". /etc/profile; puppet-compile";
		$cmd .= " -t $latexOpts -N \"$srcFile\" -o '$pc_opts' -- -\" <$testDir/$srcFile";
	}
	my $nothing =`$cmd >$outFile.tex 2>$outFile.tex.err`;
	my $status = $?;

	if ($status==0) {
		if ($verbose>1) {
			my $msg=`cat $outFile.tex`;
			print stderr "$msg\n";
		}
		unlink("$outFile.tex.err");
		print stderr "OK: Latex generated for $srcFile\n";
	} else {
		my $msg=`cat $outFile.tex.err`;
		print stderr "** FAIL: Latex generation failed for $srcFile\n$msg";
	}
}

##########################################################################################
sub CompareCatalogs($$$;$) {
##########################################################################################

	my $unordered = shift;
	my $cat1 = shift;
	my $cat2 = shift;
	my $path = shift || "";

	if (ref $cat1 eq '') {
		if (ref $cat2 eq '') {
			if ($cat1 eq $cat2) { return ''; }
			else { return $path . "'$cat1' does not match '$cat2'\n"; }
		} else {
			return $path . "scalar value '$cat1' does not match " . (ref $cat2) ."\n";
		}

	} elsif (ref $cat1 eq 'ARRAY') {
		if (ref $cat2 eq 'ARRAY') {
			return $unordered ? CompareUnorderedArrays($cat1,$cat2)
				: CompareArrays($cat1,$cat2);
		} else {
			return $path . "array does not match '" . (ref $cat2) ."'\n";
		}

	} elsif (ref $cat1 eq 'HASH') {
		if (ref $cat2 eq 'HASH') {
			return CompareHash($cat1,$cat2);
		} else {
			return $path . "hash does not match '" . (ref $cat2) ."'\n";
		}

	} elsif (defined($cat1)) {
		return $path . "unexpected type: '" . (ref $cat1) ."'\n";

	} else {
		return $path . "undefined\n";
	}
}

sub CompareArrays($$;$) {

	my $cat1 = shift;
	my $cat2 = shift;
	my $path = shift || "";

	my ($l1,$l2) = ( scalar @$cat1, scalar @$cat2 );
	if ( $l1 != $l2 ) {
		return $path . "arrays have different lengths: $l1 /= $l2";
	}
	my $msgs=''; for (my $i=0; $i<$l1; ++$i) {
		my ($e1,$e2) = ($cat1->[$i],$cat2->[$i]);
		$msgs .= CompareCatalogs(0,$e1,$e2,"$path\[$i\] -> ");
	}

	return $msgs;
}

sub CompareUnorderedArrays($$;$) {

	my $cat1 = shift;
	my $cat2 = shift;
	my $path = shift || "";

	my ($l1,$l2) = ( scalar @$cat1, scalar @$cat2 );
	if ( $l1 != $l2 ) {
		return $path . "(unordered) catalogs have different lengths: $l1 /= $l2";
	}
	my $cat2hash={}; for (my $i=0; $i<$l1; ++$i) {
		$cat2hash->{$i} = $cat2->[$i];
	}
	my $msgs=''; for (my $i1=0; $i1<$l1; ++$i1) {
		my $foundmatch = 0;
		my $e1 = $cat1->[$i1];
		foreach my $i2 (keys %$cat2hash) {
			my $e2 = $cat2hash->{$i2};
			if ( CompareCatalogs(0,$e1,$e2) == '' ) {
				delete $cat2hash->{$i2};
				$foundmatch = 1;
				last;
			}
		}
		unless ($foundmatch) {
			return $path . "(unordered) catalogs do not match\n";
		}
	}

	return $msgs;
}

sub CompareHash($$;$) {

	my $cat1 = shift;
	my $cat2 = shift;
	my $path = shift || "";

	my $msgs = '';

	foreach my $k (keys(%$cat1)) {
		if (exists ($cat2->{$k})) {
			$msgs .= CompareCatalogs(0,$cat1->{$k},$cat2->{$k},"$path"."$k -> ")
		} else {
			$msgs .=  $path . "key '$k' in one hash, but not the other\n";
		}
	}

	foreach my $k (keys(%$cat2)) {
		if (!exists ($cat1->{$k})) {
			$msgs .=  $path . "key '$k' in one hash, but not the other\n";
		}
	}

	return $msgs;
}

##########################################################################################
sub DoSummary() {
##########################################################################################

	print stderr "\n====> $fails test(s) failed, $oks passed.\n";

	# summarize the tests
	if ($testSummary>0) {
		print stderr "\nTest Summary by Feature ...\n";
		# summary by feature
		foreach my $keyword (sort (keys %$featureTable)) {
			my $s = $featureTable->{$keyword};
			my ($p,$f,$u) = ($s->{'PASS'},$s->{'FAIL'},$s->{'UNSUPPORTED'});
			my ($pk,$fk,$uk) = (scalar @$p, scalar @$f, scalar @$u);
			my $tk = $pk+$fk+$uk;
			print stderr "====> $keyword: $tk ($pk pass, $uk unsupported, $fk fail)\n";
			if ($testSummary>1) {
				my $tests = [];
				foreach my $t (@$p) { push @$tests, $t; }
				foreach my $t (@$u) { push @$tests, "$t (unsupported)"; }
				foreach my $t (@$f) { push @$tests, "$t (**fail**)"; }
				foreach my $t (sort @$tests) { print stderr "-- $t\n"; }
			}
		}
	}

	# update the latex summary
	if (defined($summaryFile)) {
		print stderr "\nUpdating Latex Summary: $summaryFile ...\n";
		unless (open(FP,"<$summaryFile")) {
			print stderr "failed to open: $summaryFile\n$!\n\n";
			return;
		}
		my $used={}; my $text=""; while (<FP>) {
			my $line = $_; chomp $line;

			if ( $line =~ /^
				( ((\\&)|[^&])* )&
				( ((\\&)|[^&])* )&
				( ((\\&)|[^&])* )&
				( [^\\\%*]* )
				([\\\s]*\%\s*\@FEATURES?\s*)
				(.*)
			$/x ) {
				my ($pre,$t,$p,$u,$post,$comment)=($1,$4,$7,$10,$11,$12);
				unless ($comment =~ /^\s*([a-zA-Z0-9_]+)\s*$/) {
					print stderr "** INVALID FEATURE: '$comment'\n";
					$text .= "$line\n";
					next;
				}
				my $keyword = $1;
				$t =~ s/^\s*//; $t =~ s/\s*$//;
				$p =~ s/^\s*//; $p =~ s/\s*$//;
				$u =~ s/^\s*//; $u =~ s/\s*$//;
				my $f = $featureTable->{$keyword};
				if (defined($f)) {
					my ($np,$nf,$nu) = ($f->{'PASS'},$f->{'FAIL'},$f->{'UNSUPPORTED'});
					my ($npk,$nfk,$nuk) = (scalar @$np, scalar @$nf, scalar @$nu);
					my $ntk = $npk+$nfk+$nuk;
					print stderr "OK: $keyword: ($t/$p/$u) -> ($ntk,$npk,$nuk)\n";
					$text .= "$pre\& $ntk \& $npk \& $nuk $post$comment\n";
					$used->{$keyword}=1;
				} else {
					print stderr "** NO TESTS: $keyword: ($t/$p/$u)\n";
					$text .= "$pre\& - \& - \& - $post$comment\n";
				}
			} else { $text .= "$line\n"; }

		}
		close(FP);

		# check for unused keywords (possible mistakes)
		foreach my $keyword (sort (keys %$featureTable)) {
			next if (defined($used->{$keyword}));
			print stderr "** NOT USED: $keyword\n";
		}

		# update the file
		unlink("$summaryFile~");
		unless (rename "$summaryFile", "$summaryFile~") {
			print stderr "$prog: failed to rename: $summaryFile -> $summaryFile~\n";
			print stderr "$prog: $!\n";
			exit(1);
		}
		unless (open FP, ">$summaryFile") {
			print stderr "$prog: failed to write: $summaryFile\n";
			print stderr "$prog: $!\n";
			exit(1);
		}
		print FP $text;
		close(FP);
	}
}

##########################################################################################
# main
##########################################################################################

my $sources = GetArguments();

# find directories
my $baseDir = `pwd`; chomp $baseDir;
$testDir = "$baseDir/Test";
$binDir = "$baseDir/Bin";
$tmpDir = "$baseDir/Tmp";
$cacheDir = "$baseDir/Cache";
$vmDir = "$baseDir/Vagrant/$vm";
foreach my $d ($testDir, $binDir, $tmpDir, $cacheDir) {
	unless (-d $testDir) {
		print stderr "$prog: can't find expected subdirectory: $d\n";
		exit(1);
	}
}

# if you run this e.g. from a Sublime (editor) menu, you can use the
# command "SRCFILE=$file make ...." so this will end up running
# the tests on whatever source file you happen to be editing at the time ...

if (!defined($sources->[0])) {
	if (defined($inFile)) {
		if ($inFile =~ /^$testDir\/(.*)$/) {
			$sources = [$1];
			my $result=`echo "$inFile" >$tmpDir/.last_test 2>/dev/null`;
		} else {
			my $lastFile =`cat $tmpDir/.last_test 2>/dev/null`; chomp $lastFile;
			if ($lastFile =~ /^$testDir\/(.*)$/) {
				$sources = [$1];
			} else {
				print stderr "$prog: current editor window is not a test file:\n";
				print stderr "$prog: $inFile\n";
				exit(0);
			}
		}
	}
}

if (!defined($sources->[0])) {
	my @allSources = split /[ \t\n]+/,`cd $testDir; find . -name '*.pp' |sed 's^./^^'`;
	$sources =\@allSources;
}

# run all the tests ...
foreach my $s (@$sources) { RunTest($s); }

# summary
DoSummary() unless ($latexOutput);
