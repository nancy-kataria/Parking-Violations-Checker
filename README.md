# Prolog-Based Reasoning on Parking Violations

This project is an interactive **parking violation checker** for CSUF, powered by a Prolog reasoning engine and a simple web frontend.

Users answer a short sequence of questions and the system uses Prolog rules to decide when it can be a parking violation.

## 1. Requirements

- [SWI-Prolog](https://www.swi-prolog.org/download/stable) (tested with 9.x)
- A modern browser (Chrome, Edge, or Firefox)

No Node, npm, or frameworks are required – the frontend is a plain HTML file.

---

## 2. Project Structure

```text
.
├── student_parking.pl      % student parking rules & lots
├── faculty_parking.pl      % faculty/staff parking rules & lots
├── overnight_parking.pl    % overnight parking rules
├── parking_logic.pl        % decision tree + next_step/2 API
├── server.pl               % SWI-Prolog HTTP server
└── index.html              % sliding-card frontend UI
```

## 3. Running the App

1. Run the server

```
swipl -s server.pl -g start
```

2. Open http://localhost:4000/ to access the working app


## Example Queries

```
?- may_park_overnight(s8, time(mon, 2), false).
?- overnight_violation(s8, time(mon, 2), false, 5, Reason).
?- student_parking_violation(s8, 11, true, false,regular_space, none, none, none,blue, true, none, false,Reason).
?- reparking_violation(f1_stall1, f1, 45, true).
?- allowed_to_park(e7, X)
```

