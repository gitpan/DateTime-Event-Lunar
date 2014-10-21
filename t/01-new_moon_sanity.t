#!perl
use strict;
use Test::More qw(no_plan);

# this is a real hard module to test. as such, we'll simply
# pick up random new moon dates, and make sure that the
# dates are not more than plus or minus T units off

BEGIN
{
	use_ok("DateTime::Event::Lunar");
}

use constant MAX_DELTA_MINUTES => 10;

# XXX - TODO: add more ?
my @dates = (
	[ 2003, 12, 23, 9, 43 ],
	[ 2005, 8, 5, 3, 5 ]
);

foreach my $d_data (@dates) {
	my %args;

	@args{ qw(year month day hour minute time_zone) } = ( @$d_data, 'UTC' );
	my $dt = DateTime->new(%args);

	# if $dt is a new moon, 7 days prior to this date is *definitely*
	# after the last new moon, but before the one expressed by $dt
	my $dt0 = $dt - DateTime::Duration->new(days => 7);

	my $new_moon = DateTime::Event::Lunar->new_moon();
	my $next_new_moon = $new_moon->next($dt0);

	check_deltas($dt, $next_new_moon);

	# Same as before, but now we try $dt + 7 days
	my $dt1 = $dt + DateTime::Duration->new(days => 7);
	my $prev_new_moon = $new_moon->previous($dt1);

	check_deltas($dt, $prev_new_moon);
}

sub check_deltas
{
	my($expected, $actual) = @_;

	my $diff = $expected - $actual;
	ok($diff);

	# make sure the deltas do not exceed 3 hours
	my %deltas = $diff->deltas;
	ok( $deltas{months} == 0 &&
		$deltas{days} == 0 &&
		abs($deltas{minutes}) < MAX_DELTA_MINUTES) or
	diag( "Expected new moon date was " . 
		$expected->strftime("%Y/%m/%d %T") . " but instead we got " .
		$actual->strftime("%Y/%m/%d %T") .
		" which is more than allowed delta of " .
		MAX_DELTA_MINUTES . " minutes" );
}
	
