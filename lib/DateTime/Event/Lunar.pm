package DateTime::Event::Lunar;
use strict;
use vars qw($VERSION);
BEGIN {
	$VERSION = '0.01';
}
use DateTime;
use DateTime::Set;
use DateTime::Util::Calc qw(
    bf_downgrade min max moment search_next moment dt_from_moment
	mod binary_search bigfloat
);
use DateTime::Util::Astro::Moon qw(MEAN_SYNODIC_MONTH);
use Math::Round qw(round);
use Params::Validate();

sub _new
{
    my $class = shift;
    return bless { _firstcall => 1 }, $class;
    
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
    my $phase = shift;
    my $self  = $class->_new(@_);
    return DateTime::Set->from_recurrence(
        next     => sub {
			$self->lunar_pharse_after(
				datetime    => $_[0],
				phase       => $phase,
			)
		},
        previous => sub {
			$self->lunar_pharse_before(
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

	my $firstcall = 
		(ref($self) && UNIVERSAL::isa($self, __PACKAGE__)) ?
			delete $self->{_firstcall} :
			1;

    my $t0  = DateTime::Util::Astro::Moon::nth_new_moon(0);
    my $phi = DateTime::Util::Astro::Moon::lunar_phase($dt);
    my $n = round( (moment($dt) - moment($t0)) / MEAN_SYNODIC_MONTH - 
        $phi / 360 );

	# if firstcall, this is new_moon_ON_OR_BEFORE. otherwise
	# it's new_moon_BEFORE
	my $checksub = $firstcall ?
		sub { DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($_[0])) <= $dt } :
		sub { DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($_[0])) < $dt };
    my $rv = search_next(
        base  => $n,
		check => $checksub,
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

	my $firstcall = 
		(ref($self) && UNIVERSAL::isa($self, __PACKAGE__)) ?
			delete $self->{_firstcall} :
			1;

    my $t0  = DateTime::Util::Astro::Moon::nth_new_moon(0);
    my $phi = DateTime::Util::Astro::Moon::lunar_phase($dt);
    my $n = round( (moment($dt) - moment($t0)) / MEAN_SYNODIC_MONTH - 
        $phi / 360 );

	# if firstcall, this is new_moon_ON_OR_AFTER. otherwise
	# it's new_moon_AFTER
	my $checksub = $firstcall ?
		sub { DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($_[0])) >= $dt } :
		sub { DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($_[0])) > $dt };
    my $rv = search_next(
        base  => $n,
        check => $checksub
    );
    return DateTime::Util::Astro::Moon::nth_new_moon(bf_downgrade($rv));
}

# [1] p.192
sub lunar_phase_before
{
    my $self = shift;
	my %args = Params::Validate::validate(@_, {
		datetime => { isa => 'DateTime' },
		phase    => { type => Params::Validate::SCALAR() },
	});
    my($dt, $phi) = ($args{datetime}, $args{phase});

	my $firstcall = 
		(ref($self) && UNIVERSAL::isa($self, __PACKAGE__)) ?
			delete $self->{_firstcall} :
			1;

    my $epsilon = 10 ** -5;
    my $tau     = moment($dt) - (bigfloat(1) / 360) * MEAN_SYNODIC_MONTH *
        mod(DateTime::Util::Astro::Moon::lunar_phase($dt) - $phi, 360);
    my $l       = $tau - 2;
    my $u       = min(moment($dt), $tau + 2);

    my $rv = binary_search($l, $u,
        sub { abs($_[0] - $_[1]) <= $epsilon },
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

	my $firstcall = 
		(ref($self) && UNIVERSAL::isa($self, __PACKAGE__)) ?
			delete $self->{_firstcall} :
			1;

    my $epsilon = 10 ** -5;
    my $tau     = moment($dt) + (bigfloat(1) / 360) * MEAN_SYNODIC_MONTH *
        mod($phi - DateTime::Util::Astro::Moon::lunar_phase($dt), 360);
    my $l       = max(moment($dt), $tau - 2);
    my $u       = $tau + 2;

    my $rv = binary_search($l, $u,
        sub { abs($_[0] - $_[1]) <= $epsilon },
        sub { mod(DateTime::Util::Astro::Moon::lunar_phase(
            dt_from_moment($_[0])) - $phi, 360) < 180 } );
	return dt_from_moment(bf_downgrade($rv));
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

  my $lunar_phase = DateTime::Event::Lunar->lunar_phase($phase);
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

=head2 DateTime::Event::Lunar-E<gt>new_moon_after($dt)

Returns a DateTime object representing the next new moon, which may fall
on the same date given by $dt. 

  my $next_dt = DateTime::Event::Lunar->new_moon_after(datetime => $dt0);

This is the function that is internally used by new_moon()-E<gt>next().
Note that in order to make DateTime::Span work, this function works slightly
differently depending on the timing that it is called. Namely, the first
time (or when it's called as a class method) it's called, this function
effectively calculates new moon B<on or after> the given date. However,
from the second call on, it calculates the next new moon, not including
the given date.

=head2 DateTime::Event::Lunar-E<gt>new_moon_before(%args)

Returns a DateTime object representing the previous new moon, which may fall
on the same date given by $dt:

  my $prev_dt = DateTime::Event::Lunar->new_moon_before(datetime => $dt0);

This is the function that is internally used by new_moon()-E<gt>previous().
Note that in order to make DateTime::Span work, this function works slightly
differently depending on the timing that it is called. Namely, the first
time (or when it's called as a class method) it's called, this function
effectively calculates new moon B<on or before> the given date. However,
from the second call on, it calculates the previous new moon, not including
the given date.

=head2 DateTime::Event::Lunar-E<gt>lunar_phase($phase)

Returns a DateTime::Set object that you can use to get the date of the
next or previous date, when the lunar longitude is at $phase degrees

  my $set = DateTime::Event::Lunar->lunar_phase(60);
  my $dt  = DateTime->now();
  my $dt_at_longitude_60 = $set->next($dt);

=head1 CAVEATS

Spansets created via intersection() functions are *very* slow at first,
because it needs to calculate all the possible values within the span
first. If you are going to be using these values in different places,
it is strongly suggested that you create one spanset before hand that
others can refer to.

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
