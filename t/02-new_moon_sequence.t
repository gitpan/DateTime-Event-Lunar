#!perl
use strict;
use Test::More qw(no_plan);
BEGIN
{
    use_ok("DateTime::Event::Lunar");
}

my $dt = DateTime->now();
$dt->set_time_zone('UTC');

my $new_moon = DateTime::Event::Lunar->new_moon();

my $next_new_moon = $new_moon->next($dt);

check_sequence($dt, $new_moon);
check_sequence($next_new_moon, $new_moon);

sub check_sequence
{
    my($dt, $new_moon) = @_;
    my(@ascending, @descending);

diag "  Generating new moons... Please hold.";

    my $count = 5;
    foreach (0..$count) {
        $dt = $new_moon->next($dt);
        push @ascending, $dt;
    }
    
    foreach (0..$count) {
        $dt = $new_moon->previous($dt);
        push @descending, $dt;
    }
    
    # don't compare the last one, so $count - 1 
    for(0..$count - 1) {
        my $asc_dt = $ascending[$_];
        my $dec_dt = $descending[4 - $_];
        my $delta  = $asc_dt->delta_ms($dec_dt);
    
        ok($delta->minutes() < 1);
    }
}
