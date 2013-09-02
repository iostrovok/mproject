#!/usr/bin/perl

use utf8;
use strict;
use warnings;
use feature qw(say);

use Getopt::Long;
use FindBin qw[ $Bin ];

say "\$Bin = $Bin\n";

my $ejson_bin_dir = "$Bin/ejson/bin/";
my $ejson_dir = "$Bin/ejson/";
my $bin_dir = "$Bin/ejson/";
my ( $install_rebar, $install_ejson, $install_memcache );

my $result = GetOptions (
		"r=i"       => \$install_rebar,
		"ej=i"      => \$install_ejson,
		"mem=i"     => \$install_memcache,
	);

$install_rebar = 1 unless defined $install_rebar;
$install_ejson = 1 unless defined $install_ejson;
$install_memcache = 1 unless defined $install_memcache;


if ( $install_rebar ) {
    system('git clone https://github.com/basho/rebar.git');
    system('cd rebar && make');
    system('cp ./rebar/rebar ./');
}

if ( $install_ejson ) {
    system('git clone https://github.com/davisp/ejson.git');
    system('cp ./rebar/rebar ./ejson/');
    system('cd ./ejson && make');
}

if ( $install_memcache ) {
    system('git clone https://github.com/EchoTeam/mcd.git');
    system('cp ./rebar/rebar ./mcd/');
    system('cd ./mcd && make');
}

system('git clone https://github.com/iostrovok/mproject.git');



#erl -pa apps/*/ebin -boot start_sasl -s dummy_proj

__END__

# Install main code now

system('rm ./*/*.beam');

https://github.com/benoitc/ejson.git

git clone --bare https://github.com/benoitc/ejson.git
https://github.com/benoitc/ejson.git

mkdir './ejson' unless -d './ejson';

chroot ''




say /Users/ostrovok/Projects/Proxy/

cd /Users/ostrovok/Projects/Proxy/
say Removing
for f in $( ls ./*/*.beam ); do
    rm $f
done;

cd /Users/ostrovok/Projects/Proxy/src/
say Compiling
for f in $( ls ./*.erl ); do
    say \.\.\.$f
    erlc -Wall -v +debug_info -o /Users/ostrovok/Projects/Proxy/ebin $f
done;

chmod -R 777 /Users/ostrovok/Projects/Proxy/

say /Users/ostrovok/erl_lib/ejson/
cd /Users/ostrovok/erl_lib/ejson/
say Removing
for f in $( ls ./*/*.beam ); do
    rm $f
done;
for f in $( ls ./*.beam ); do
    rm $f
done;
say Compiling
for f in $( ls ./src/*.erl ); do
    say \.\.\.$f
    erlc -Wall -v +debug_info -o ./ebin $f
done;

chmod -R 777 /Users/ostrovok/erl_lib/
