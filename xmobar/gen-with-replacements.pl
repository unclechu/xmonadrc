#!/usr/bin/env perl
#
# Generates xmobar config with some customizations
# taken from '~/.xmonad/xmobar.replacements.hs'
# and saves it to '~/.xmonad/xmobar.hs'.
#
# Example of '~/.xmonad/xmobar.replacements.hs':
#   {-^ position ^-}
#   position = Static { xpos = 1440, ypos = 700, width = 1080, height = 10 }
#   {-$ position $-}
#

use v5.10; use strict; use warnings; use autodie qw<:all>;
use File::Basename qw<dirname>;
use Cwd qw<abs_path>;
use Env qw<HOME>;

close STDIN;
my $dir = abs_path dirname __FILE__;
my $src_file = "$dir/xmobar.hs";
my $dest_file = "$HOME/.xmonad/xmobar.generated.hs";
my $replacements_file = "$HOME/.xmonad/xmobar.replacements.hs";
my $replacement_reg_begin = qr/^{-\^ ([a-zA-Z]+) \^-}$/;
my $replacement_reg_end = qr/^{-\$ ([a-zA-Z]+) \$-}$/;

sub trim {
	my $by_arg = scalar(@_) > 0;
	my $x = $by_arg ? $_[0] : $_;
	$x =~ s/(^\s+|\s+$)//g;
	$_ = $x unless $by_arg;
	$x;
}

if (scalar(@ARGV) == 1 && $ARGV[0] eq '--clean') {
	unlink $dest_file if -e $dest_file;
	exit 0;
} elsif (scalar(@ARGV) != 0) {
	say STDERR "Incorrect arguments: @ARGV";
	exit 1;
}

my %replacements = ();

sub preprocess {

	my @res = (
		'-- This file was generated automatically.',
		'-- Please, do not edit, it could be overwritten.',
		'-- Use ~/.xmonad/xmobar.replacements.hs if you need to customize it.',
		'',
	);

	my $current_replacement;
	my @default_value = ();

	open my $fh, '<', $src_file;
	chomp(my @src_lines = <$fh>);
	@src_lines = grep { $_ !~ /(^--|^$)/ } map {trim} @src_lines;

	foreach (@src_lines) {

		if ($_ =~ $replacement_reg_begin) {

			if (defined $current_replacement) {

				say STDERR
					'Preprocessing error: ' .
					"Found opening of '$1' replacement " .
					"but previously opened '$current_replacement' " .
					'replacement is not closed yet';

				exit 1;
			}

			$current_replacement = $1;

		} elsif ($_ =~ $replacement_reg_end) {

			unless (defined $current_replacement) {

				say STDERR
					'Preprocessing error: ' .
					"Found closing of '$1' replacement " .
					'but it was not previously opened';

				exit 1;

			} elsif ($current_replacement ne $1) {

				say STDERR
					'Preprocessing error: ' .
					"Found closing of '$1' replacement " .
					"but previously opened was '$current_replacement'";

				exit 1;
			}

			push @res, exists($replacements{$1}) ?
				@{$replacements{$1}} : @default_value;

			@default_value = ();
			$current_replacement = undef;

		} elsif (defined $current_replacement) {
			push @default_value, $_;
		} else {
			push @res, $_;
		}
	}

	if (defined $current_replacement) {

		say STDERR
			'Preprocessing error: ' .
			"It's EOF but replacement '$current_replacement' isn't closed";

		exit 1;
	}

	@res;
}

sub generate_and_save {
	unlink $dest_file if -e $dest_file;
	my @lines = preprocess;
	open my $fh, '>', $dest_file;
	say $fh join "\n", @lines;
	close $fh;
}

my $current_replacement;
open my $fh, '<', $replacements_file;
chomp(my @r_lines = <$fh>);
@r_lines = grep { $_ !~ /(^--|^$)/ } map {trim} @r_lines;

foreach (@r_lines) {

	if ($_ =~ $replacement_reg_begin) {

		if (defined $current_replacement) {

			say STDERR
				"Extracting replacement from '$replacements_file' error: " .
				"Found opening of '$1' replacement " .
				"but previously opened '$current_replacement' " .
				'replacement is not closed yet';

			exit 1;
		}

		$current_replacement = $1;
		$replacements{$1} = [];

	} elsif ($_ =~ $replacement_reg_end) {

		unless (defined $current_replacement) {

			say STDERR
				"Extracting replacement from '$replacements_file' error: " .
				"Found closing of '$1' replacement " .
				'but it was not previously opened';

			exit 1;

		} elsif ($current_replacement ne $1) {

			say STDERR
				"Extracting replacement from '$replacements_file' error: " .
				"Found closing of '$1' replacement " .
				"but previously opened was '$current_replacement'";

			exit 1;
		}

		$current_replacement = undef;

	} elsif (defined $current_replacement) {

		push @{$replacements{$current_replacement}}, $_;
	}

	# Any other non-replacement lines will be ignored
}

if (defined $current_replacement) {

	say STDERR
		"Extracting replacement from '$replacements_file' error: " .
		"It's EOF but replacement '$current_replacement' isn't closed";

	exit 1;
}

generate_and_save;
