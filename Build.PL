# $Id: Build.PL,v 1.4 2005/01/08 02:25:43 lestrrat Exp $
#
# Daisuke Maki <dmaki@cpan.org>
# All rights reserved.

use strict;
use Module::Build;

my $class = Module::Build->subclass(code => q|
    sub ACTION_manifest
    {
        my $self = shift;
        $self->SUPER::ACTION_manifest();
        if ($self->create_makefile_pl) {
            $self->_add_to_manifest('MANIFEST', 'Makefile.PL        Added here by Build.PL');
        }
    }
|);

my $build = $class->new(
    module_name => 'DateTime::Event::Lunar',
    license => 'perl',
    requires => {
        'DateTime' => 0,
        'DateTime::Set' => 0,
        'DateTime::Util::Calc' => '0.08',
        'DateTime::Util::Astro::Common' => '0.02',
        'DateTime::Util::Astro::Moon' => '0.04',
        'DateTime::Util::Astro::Sun' => '0.03',
        'Math::Round' => 0,
        'Params::Validate' => 0,
    },
    sign => 1,
    create_makefile_pl => 'traditional'
);
$build->create_build_script;
