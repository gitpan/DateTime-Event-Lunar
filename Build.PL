use strict;
use Module::Build;

my $build = Module::Build->new(
	module_name => 'DateTime::Event::Lunar',
	license => 'perl',
	requires => {
		'DateTime' => 0,
		'DateTime::Set' => 0,
		'DateTime::Util::Calc' => 0,
		'DateTime::Util::Astro::Moon' => 0,
		'DateTime::Util::Astro::Sun' => 0,
		'Math::Round' => 0,
		'Params::Validate' => 0,
	},
	recommends => {
		'perl' => '5.6.1',
		'Memoize' => 0,
		'Math::BigInt::GMP' => 0,
	},
	sign => 1,
	create_readme => 1,
    create_makefile_pl => 'passthrough'
);
$build->create_build_script;
