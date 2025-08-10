<?php

declare(strict_types=1);

// leveraging built-in functions instead of doing calcs myself
function meetup_day(int $year, int $month, string $which, string $weekday): DateTimeImmutable {
    $months = [ '', 'January', 'February', 'March', 'April', 'May', 'June', 
                'July', 'August', 'September', 'October', 'November', 'December'];
    if ($which == 'teenth') {
        $x = new DateTimeImmutable('first ' . $weekday . ' of ' . $months[(int) $month] . ' ' . $year);
        return (int) $x->format('d') < 6 ? $x->add(new DateInterval('P2W')) : $x->add(new DateInterval('P1W'));
    }
    return new DateTimeImmutable($which . ' ' . $weekday . ' of ' . $months[$month] . ' ' . $year);
}