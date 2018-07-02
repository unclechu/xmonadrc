#!/usr/bin/env perl
use v5.10; use strict; use warnings; use autodie qw(:all);
die 'unexpected arguments count' if scalar(@ARGV) != 1;
use Env qw<HOME>;
use Cwd qw<cwd abs_path>;
use IPC::System::Simple qw<runx>;
use File::Slurp qw<slurp>;


if ($ARGV[0] eq 'xmonad') {

  chdir 'xmonad';
  runx qw<stack build --install-ghc>;
  runx qw<stack install>;

} elsif ($ARGV[0] eq 'clean-xmonad') {

  chdir 'xmonad';
  runx qw<stack clean>;
}


elsif ($ARGV[0] eq 'xmobar' || $ARGV[0] eq 'clean-xmobar') {

  chdir 'xmobar';
  my $xmobar_dir = abs_path cwd;

  # creating `xmobar.replacements.hs` if it doesn't exists in ~/.xmonad/
  chdir $HOME;
  my $local_xmonad_dir = '.xmonad';
  mkdir $local_xmonad_dir unless -d $local_xmonad_dir;
  chdir $local_xmonad_dir;
  my $replacements_f = 'xmobar.replacements.hs';
  unless (-f $replacements_f) {
    open my $fh, '>', $replacements_f;
    say $fh "x = y { z = 1\n\n\n-- Add your replacements blocks here\n\n\n}";
    close $fh;
  }

  chdir $xmobar_dir;
  if ($ARGV[0] eq 'xmobar') {
    runx qw<./gen-with-replacements.pl>
  } elsif ($ARGV[0] eq 'clean-xmobar') {
    runx qw<./gen-with-replacements.pl --clean>
  } else {die}
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


elsif ($ARGV[0] eq 'session') {
  my $xmonadrc_dir = abs_path cwd;

  # making symbolic link to `xmonad-unclechu-session.sh` (local session runner)
  chdir $HOME;
  my $local_xmonad_dir = '.xmonad';
  mkdir $local_xmonad_dir unless -d $local_xmonad_dir;
  chdir $local_xmonad_dir;
  my $sh_f = 'xmonad-unclechu-session.sh';
  runx qw<ln -s -->, "$xmonadrc_dir/session/$sh_f" unless -f $sh_f;

  # copying session *.desktop file for a session manager
  my $xsessions_dir = '/usr/share/xsessions';
  my $session_f = 'xmonad-unclechu.desktop';
  say
    qq/Privileges permissions will be requested to write to "$xsessions_dir" /,
    qq/directory to save "$session_f" session file there.../;
  my $sess_f_contents = slurp "$xmonadrc_dir/session/$session_f";
  my $old_home_path = '/home/unclechu';
  $sess_f_contents =~ s/$old_home_path/$HOME/g;
  open my $fh, qq{| sudo tee -- "$xsessions_dir/$session_f" >/dev/null};
  print $fh $sess_f_contents;
}


else {
  die "unknown argument: '$ARGV[0]'";
}

# vim: et ts=2 sts=2 sw=2 cc=81 tw=80 :
