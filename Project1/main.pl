:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).
:- consult('menu.pl').
:- consult('board.pl').

start :-
    play.
