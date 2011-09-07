#!/usr/bin/perl
# ============================================================================
# $Id: genTZPacking.pl 19162 2008-06-03 20:59:32Z stever $
# this script is used to generate the section of PACKING.be for WebTools.
#
# ============================================================================

open OUT, ">www.packing" or die "$0: can't open www.packing for write";
print OUT "mkdir examples/www\n";
for $file (`find . -type d | grep -v '.svn'`) {
  chomp $file;
  $file =~ s/^\.\///;
  next if $file eq ".";
  push @dirs, $file; # save these for later
  print OUT "mkdir examples/www/$file\n";
}

for $file (`find . -type f | grep -v '.svn' | grep -v 'www.packing' | grep -v 'genPacking.pl'`) {
  chomp $file;
  my $isAscii = -T $file;
  $file =~ s/^\.//;
  print OUT "copy examples/www$file	\$ARCHBASE/examples/www$file	";
  print OUT $isAscii ? "text\n" : "bin\n"; 
}

for $dir (@dirs) {
  print OUT "chmod examples/www/$dir	dir	555\n";
}
print OUT "chmod examples/www	dir	555\n";

