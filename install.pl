#!/usr/bin/perl

use utf8;
use strict;
use warnings;
use feature qw(say);

use Getopt::Long;
use File::Temp qw/ tempfile tempdir /;
use FindBin qw[ $Bin ];

my $MPROJECT_REPO   = 'https://github.com/iostrovok/mproject.git';
my $REBAR_REPO      = 'https://github.com/basho/rebar.git';
my $EJSON_REPO      = 'https://github.com/davisp/ejson.git';
my $MCD_REPO        = 'https://github.com/EchoTeam/mcd.git';

my ( $install_rebar, $install_ejson, $install_memcache, $install_mproject );
my $result = GetOptions (
		"r=i"       => \$install_rebar,
		"ej=i"      => \$install_ejson,
		"mem=i"     => \$install_memcache,
        "m=i"       => \$install_mproject,
	);

$install_rebar    = 1 unless defined $install_rebar;
$install_ejson    = 1 unless defined $install_ejson;
$install_memcache = 1 unless defined $install_memcache;
$install_mproject = 1 unless defined $install_mproject && $install_mproject == 0;
$install_rebar    = 1 if $install_memcache;

my $rebar_dir = tempdir( CLEANUP => 1 );

say "\$Bin = $Bin\n";
my $Dir = "$Bin/mproject";
my $ebin_dir  = "$Dir/ebin";
my $src_dir   = "$Dir/src";
my $ejson_dir = "$Dir/ejson";
#my $rebar_dir = "$Dir/rebar";
my $mem_dir   = "$Dir/mcd/";

get_rebar() if $install_rebar;

if ( $install_mproject ) {
    get_mproject();
}
else {
    mkdir($Dir);
    chdir($Dir) or die "Can't create dir $Dir";
}

mkdir($ebin_dir) unless -d $ebin_dir;

get_ejson() if $install_ejson;
get_mcd()   if $install_memcache;

imake("ln -s $src_dir/mproxy_app.app $ebin_dir/mproxy_app.app");
imake(q[ export ERL_LIBS="$Dir" ]);
imake(q[ set ERL_LIBS="$Dir" ]);


exit();

##############################################

sub get_mproject {
    say "Start install mcd in $Dir";
    chdir($Bin);

    # Get main repo
    imake("git clone $MPROJECT_REPO");
    chdir($Dir) or die "No download main repo from $MPROJECT_REPO";

    mkdir($ebin_dir) unless -d $ebin_dir;

    opendir( DIR, $src_dir) or die "$@";
    while (my $f = readdir DIR ) {
        say $f;
        next unless $f =~ m/\.erl$/;
        say("erlc -Wall -v +debug_info -o $ebin_dir $src_dir/$f");
        imake("erlc -Wall -v +debug_info -o $ebin_dir $src_dir/$f");
    }
    closedir DIR;

    #imake("cp $rebar_dir/rebar $Dir");
    # finish main repo
}

sub get_rebar {
    say "Start install rebar in $rebar_dir";
    chdir($rebar_dir);
    imake("git clone $REBAR_REPO/ ./");
    die "No download rebar repo from $REBAR_REPO" unless -d $rebar_dir;
    imake("cd $rebar_dir && make");
    die "Reabar compile error" unless -f "$rebar_dir/rebar";
    imake("cp $rebar_dir/rebar $Bin");
}

sub get_ejson {
    say "Start install ejson in $ejson_dir";
    chdir($Dir);
    imake("git clone $EJSON_REPO");
    die "No download ejaon repo from $EJSON_REPO" unless -d $ejson_dir;
    #imake("cp $rebar_dir/rebar $ejson_dir");
    imake("cd $ejson_dir && make");
}

sub get_mcd {
    say "Start install mcd in $mem_dir";
    chdir($Dir);
    imake("git clone $MCD_REPO");
    die "No download mcd repo from $MCD_REPO" unless -d $mem_dir;
    imake("cp $rebar_dir/rebar $mem_dir");
    imake("cd $mem_dir && make");
}

sub imake {
    my $line = shift;
    say "exe: $line";
    system($line);
}

#erl -pa apps/*/ebin -boot start_sasl -s dummy_proj
#erl -pa mproject/*/ebin -s dummy_proj

__END__

git clone ssh://git@github.com:iostrovok/mproject.git
git push ssh://git@github.com:iostrovok/mproject.git

