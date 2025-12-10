:- module(server, [start/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).

:- ensure_loaded(parking_logic).     % your logic file

% Serve the frontend at "/"
:- http_handler(root(.), index_handler, []).

% JSON API at "/next"
:- http_handler(root(next), next_handler, [method(post)]).

start :-
    http_server(http_dispatch, [port(4000)]).

index_handler(Request) :-
    % serve index.html from current directory
    http_reply_file('index.html', [], Request).

next_handler(Request) :-
    % Convert all JSON strings into atoms so they match our parking_logic checks
    http_read_json_dict(Request, In, [value_string_as(atom)]),
    (   _{answers:Answers} :< In
    ->  true
    ;   Answers = _{}
    ),
    next_step(Answers, Response),
    reply_json_dict(Response).

