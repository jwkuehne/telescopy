#!/usr/bin/perl
# Cull non-Dreyer lines
while ($line = <>) {
   ($N, $CAT, $STAR, @REST) = split("	", $line);
   if ($STAR eq '*') {
      print "$line";
   }
   $STAR = '';
}
