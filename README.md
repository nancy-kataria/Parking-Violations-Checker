# Prolog-Based Reasoning on Parking Violations

This project is an interactive **parking eligibility checker** for CSUF, powered by a Prolog reasoning engine and a simple web frontend.

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

