:- module(overnight_parking, [
    may_park_overnight/3,
    overnight_violation/5
]).

% Facts

overnight_lot(s8).
overnight_lot(e7).

overnight_hour(Hour) :-
    integer(Hour),
    Hour >= 1,
    Hour < 6.

storage_limit_hours(72).

% Rules


%  applies during overnight hours. Outside of those
%  hours this predicate simply fails (overnight rule not applicable).

may_park_overnight(Lot, time(_Date, Hour), HasValidPermit) :-
    overnight_hour(Hour),
    overnight_lot(Lot),
    HasValidPermit == true.


overnight_violation(Lot, time(_Date, Hour), HasValidPermit, DurationHours, Reason) :-
    overnight_hour(Hour),
    overnight_violation_reason(Lot, HasValidPermit, DurationHours, Reason).

% Helper that encodes each specific overnight rule as a Reason.

overnight_violation_reason(Lot, _HasValidPermit, _DurationHours, invalid_lot) :-
    % During overnight hours, only S8 or E7 are allowed.
    \+ overnight_lot(Lot).

overnight_violation_reason(Lot, HasValidPermit, _DurationHours, missing_or_invalid_permit) :-
    overnight_lot(Lot),
    HasValidPermit \== true.

overnight_violation_reason(Lot, HasValidPermit, DurationHours, storage_limit_exceeded) :-
    overnight_lot(Lot),
    HasValidPermit == true,
    storage_limit_hours(Limit),
    DurationHours > Limit.
