:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('menu.pl').
:- consult('board.pl').

start :-
    play.
