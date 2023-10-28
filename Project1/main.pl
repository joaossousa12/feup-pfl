:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- consult('menu.pl').
:- consult('board.pl').

start :-
    play.
