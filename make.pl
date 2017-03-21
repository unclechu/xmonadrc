#!/usr/bin/env perl
use v5.10; use strict; use warnings; use autodie qw(:all);
die 'unexpected arguments count' if scalar(@ARGV) != 1;
use Env qw<HOME>;
use IPC::System::Simple qw<runx>;


if ($ARGV[0] eq 'xmonad') {

  chdir 'xmonad';
  runx qw<stack build --install-ghc>;
  runx qw<stack install>;

} elsif ($ARGV[0] eq 'clean-xmonad') {

  chdir 'xmonad';
  runx qw<stack clean>;
}


elsif ($ARGV[0] eq 'xmobar') {

  chdir 'xmobar';
  runx qw<./gen-with-replacements.sh>;

} elsif ($ARGV[0] eq 'clean-xmobar') {

  chdir 'xmobar';
  runx qw<./gen-with-replacements.sh --clean>;
}


elsif ($ARGV[0] eq 'test-xmobar-indicators-cmd') {

  chdir 'xmobar/indicators-cmd';
  runx qw<stack build --install-ghc>;
  runx qw<stack test>;

} elsif ($ARGV[0] eq 'xmobar-indicators-cmd') {

  chdir 'xmobar/indicators-cmd';
  runx qw<stack build --install-ghc>;
  runx qw<stack install>;

} elsif ($ARGV[0] eq 'clean-xmobar-indicators-cmd') {

  chdir 'xmobar/indicators-cmd';
  runx qw<stack clean>;
}


else {
  die "unknown argument: '$ARGV[0]'";
}

# vim: et ts=2 sts=2 sw=2 cc=81 tw=80 :
