#!perl
use strict;
use inc::Module::Install;

name('DateTime-Event-Lunar');
all_from('lib/DateTime/Event/Lunar.pm');
license('perl');

requires 'DateTime';
requires 'DateTime::Set';
requires 'DateTime::Util::Calc'  => '0.13';
requires 'DateTime::Util::Astro' => '0.09';
requires 'Math::Round';

auto_install;
WriteAll;
