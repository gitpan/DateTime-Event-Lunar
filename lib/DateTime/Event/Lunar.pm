package DateTime::Event::Lunar;
use strict;
use vars qw($VERSION @ISA %EXPORT_TAGS);
use DateTime;
use DateTime::Set;
use DateTime::Util::Calc qw(
    bf_downgrade min max moment search_next moment dt_from_moment
    mod binary_search bigfloat
);
use DateTime::Util::Astro::Moon qw(MEAN_SYNODIC_MONTH);
use Exporter;
use Math::Round qw(round);
use Params::Validate();
BEGIN {
    $VERSION = '0.01';
    @ISA     = qw(Exporter);
    %EXPORT_TAGS = (
        phases => [ qw(NEW_MOON FIRST_QUARTER FULL_MOON LAST_QUARTER) ]
    );
    Exporter::export_ok_tags('phases');
}
use constant NEW_MOON        => 0;
use constant FIRST_QUARTER   => 90;
use constant FULL_MOON       => 180;
use constant LAST_QUARTER    => 270;
use constant ZEROTH_NEW_MOON => DateTime::Util::Astro::Moon::nth_new_moon(0);

sub _new
{
    my $class = shift;
    return bless {}, $class;
}

sub new_moon
{
    my $class = shift;
    my $self  = $class->_new(@_);
    return DateTime::Set->from_recurrence(
        next     => sub { $self->new_moon_after( datetime => $_[0] ) },
        previous => sub { $self->new_moon_before( datetime => $_[0] ) }
    );
}

sub lunar_phase
{
    my $class = shift;
    my $self  = $class->_new();

    my %args  = Params::Validate::validate(@_, {
        phase => Params::Validate::SCALAR()
    } );
    my $phase = $args{phase};
    return DateTime::Set->from_recurrence(
        next     => sub {
            $self->lunar_phase_after(
                datetime    => $_[0],
                phase       => $phase,
            )
        },
        previous => sub {
            $self->lunar_phase_before(
                datetime    => $_[0],
                phase       => $phase,
            )
        }
    );
}
    

# [1] p.190
sub new_moon_before
{
    my $self = shift;
    my(%args) = Params::Validate::validate(@_, {
        datetime => { isa => 'DateTime' }
    } );
    my $dt = $args{datetime};

    my $phi = DateTime::Util::Astro::Moon::lunar_phase($dt);
    my $n = round( (moment($dt) - moment(ZEROTH_NEW_MOON)) /
        MEAN_SYNODIC_MONTH - $phi / 360 );

    my $rv = search_next(
        base  => $n,
        check => sub { DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($_[0])) < $dt },
        next  => sub { $_[0] - 1 }
    );
    return DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($rv));
}

# [1] p.190
sub new_moon_after
{
    my $self = shift;
    my(%args) = Params::Validate::validate(@_, {
        datetime => { isa => 'DateTime' }
    } );
    my $dt = $args{datetime};

    my $phi = DateTime::Util::Astro::Moon::lunar_phase($dt);
    my $n = round( (moment($dt) - moment(ZEROTH_NEW_MOON)) /
        MEAN_SYNODIC_MONTH - $phi / 360 );

    my $rv = search_next(
        base  => $n,
        check => sub { DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($_[0])) > $dt }
    );
    return DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($rv));
}

use constant LUNAR_PHASE_DELTA => 10 ** -5;

# [1] p.192
sub lunar_phase_before
{
    my $self = shift;
    my %args = Params::Validate::validate(@_, {
        datetime => { isa => 'DateTime' },
        phase    => { type => Params::Validate::SCALAR() },
    });
    my($dt, $phi) = ($args{datetime}, $args{phase});

    my $tau     = moment($dt) - (bigfloat(1) / 360) * MEAN_SYNODIC_MONTH *
        mod(DateTime::Util::Astro::Moon::lunar_phase($dt) - $phi, 360);
    my $l       = $tau - 2;
    my $u       = min(moment($dt), $tau + 2);

    my $rv = binary_search($l, $u,
        sub { abs($_[0] - $_[1]) <= LUNAR_PHASE_DELTA },
        sub { mod(DateTime::Util::Astro::Moon::lunar_phase(
            dt_from_moment($_[0])) - $phi, 360) < 180 } );
    return dt_from_moment(bf_downgrade($rv));
}

# [1] p.192
sub lunar_phase_after
{
    my $self = shift;
    my %args = Params::Validate::validate(@_, {
        datetime => { isa => 'DateTime' },
        phase    => { type => Params::Validate::SCALAR() },
    });
    my($dt, $phi) = ($args{datetime}, $args{phase});

    my $current_phase = DateTime::Util::Astro::Moon::lunar_phase($dt);

    # these values don't need to be "Math::BigFloat", so downgrade for
    # faster calculation...
    my $tau     = bf_downgrade(
        moment($dt) + (bigfloat(1) / 360) * MEAN_SYNODIC_MONTH *
        mod($phi - DateTime::Util::Astro::Moon::lunar_phase($dt), 360)
    );
    my $l       = bf_downgrade( max(moment($dt), $tau - 2) );
    my $u       = $tau + 2;

    my $rv_moment = binary_search($l, $u,
        sub { abs($_[0] - $_[1]) <= LUNAR_PHASE_DELTA },
        sub { mod(DateTime::Util::Astro::Moon::lunar_phase(
            dt_from_moment($_[0])) - $phi, 360) < 180 } );
    my $rv = dt_from_moment(bf_downgrade($rv_moment));

    # if the delta is within some amount, we've probably just calculated
    # the same date for the same lunar phase. In that case we just
    # jump ahead 28 days (which is still safely before the next
    # date/time for the given phase) and re-calculate
    my $delta = $rv->delta_ms($dt);
    if (abs($delta->delta_minutes()) < 60) {
        return $self->lunar_phase_after(
            datetime => $dt + DateTime::Duration->new(days => 28),
            phase    => $phi
        );
    } else {
        return $rv;
    }
}

1;

__END__

=head1 NAME

DateTime::Event::Lunar - Perl DateTime Extension For Computing Lunar Events

=head1 SYNOPSIS

  use DateTime::Event::Lunar;
  my $new_moon = DateTime::Event::Lunar->new_moon();

  my $dt0  = DateTime->new(...);
  my $next_new_moon = $new_moon->next($dt0);
  my $prev_new_moon = $new_moon->previous($dt0);

  my $dt1  = DateTime->new(...);
  my $dt2  = DateTime->new(...);
  my $span = DateTime::Span->new(start => $dt1, end => $dt2);

  my $set  = $new_moon->intersection($span);
  my $iter = $set->iterator();

  while (my $dt = $iter->next) {
    print $dt->datetime, "\n";
  }

  my $lunar_phase = DateTime::Event::Lunar->lunar_phase(phase => $phase);
  # same as new_moon, but returns DateTime objects
  # when the lunar phase is at $phase degress.

  # if you just want to calculate a single new moon event
  my $dt = DateTime::Event::Lunar->new_moon_after(datetime => $dt0);
  my $dt = DateTime::Event::Lunar->new_moon_before(datetime => $dt0);

  # if you just want to calculate a single lunar phase time
  my $dt = DateTime::Event::Lunar->lunar_phase_after(
        datetime => $dt0, phase => $degrees);
  my $dt = DateTime::Event::Lunar->lunar_phase_before(
        datetime => $dt0, phase => $degrees);

=head1 DESCRIPTION

This module calculates the time and date of certain recurring lunar
events, including new moons and specific lunar phases. 

Calculations for this module are based on "Calendrical Calculations" [1].
Please see REFERENCES for details.

=head2 DateTime::Event::Lunar-E<gt>new_moon()

Returns a DateTime::Set object that you can use to get the date of the
next or previous new moon.

  my $set = DateTime::Event::Lunar->new_moon();
  my $dt  = DateTime->now();
  my $dt_of_next_new_moon = $set->next($dt);

Or you can use it in conjunction with DateTime::Span. See SYNOPSIS.

=head2 DateTime::Event::Lunar-E<gt>new_moon_after(%args)

Returns a DateTime object representing the next new moon relative to the
datetime argument.

  my $next_dt = DateTime::Event::Lunar->new_moon_after(datetime => $dt0);

This is the function that is internally used by new_moon()-E<gt>next().

=head2 DateTime::Event::Lunar-E<gt>new_moon_before(%args)

Returns a DateTime object representing the previous new moon relative
to the datetime argument.

  my $prev_dt = DateTime::Event::Lunar->new_moon_before(datetime => $dt0);

This is the function that is internally used by new_moon()-E<gt>previous().

=head2 DateTime::Event::Lunar-E<gt>lunar_phase(%args)

Returns a DateTime::Set object that you can use to get the date of the
next or previous date, when the lunar longitude is at $phase degrees

  my $set = DateTime::Event::Lunar->lunar_phase(phase => 60);
  my $dt  = DateTime->now();
  my $dt_at_longitude_60 = $set->next($dt);

Or you can use it in conjunction with DateTime::Span. See SYNOPSIS.

=head2 DateTime::Event::Lunar-E<gt>lunar_phase_after(%args);

Returns a DateTime object representing the next date that the lunar
phase is equal to the phase argument, relative to the datetime argument.

  use DateTime::Event::Lunar qw(:phases);
  my $next_dt = DateTime::Event::Lunar->lunar_phase_after(
    datetime => $dt,
    phase    => FULL_MOON
  );

This is the function that is internally used by lunar_pharse()-E<gt>next()

=head2 DateTime::Event::Lunar-E<gt>lunar_phase_before(%args);

Returns a DateTime object representing the previous date that the lunar
phase is equal to the phase argument, relative to the datetime argument.

  use DateTime::Event::Lunar qw(:phases);
  my $prev_dt = DateTime::Event::Lunar->lunar_phase_before(
    datetime => $dt,
    phase    => FULL_MOON
  );

This is the function that is internally used by lunar_pharse()-E<gt>previous()

=head1 CAVEATS

Spansets created via intersection() functions are *very* slow at first,
because it needs to calculate all the possible values within the span
first. If you are going to be using these values in different places,
it is strongly suggested that you create one spanset before hand that
others can refer to.

Lunar phases are even slower than new moons. It would be nice to fix it...

=head1 AUTHOR

Daisuke Maki E<lt>daisuke@cpan.orgE<gt>

=head1 REFERENCES

  [1] Edward M. Reingold, Nachum Dershowitz
      "Calendrical Calculations (Millenium Edition)", 2nd ed.
       Cambridge University Press, Cambridge, UK 2002

=head1 SEE ALSO

L<DateTime>
L<DateTime::Set>
L<DateTime::Span>
L<DateTime::Util::Astro::Moon>
L<DateTime::Util::Astro::Sun>

=cut
