%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Computee's basic attributes and capabilities
%% by Neophytos Demetriou (nkd@ucy.ac.cy)
%%
%% This software is licensed under the Gnu Public License.
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(objects)).

:- multifile complement/2, rule/3, kb_gd__rule/3, kb_gd__todo/1.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% uid(UID): the computee's unique identifier
%%
%% timepoint(T): local clock of the computee.
%%
%% epsilon(E): is a sufficiently small number and its used to denote how much 
%% back in time should we check for effects of executed actions.
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



computee_basic :: {
     super(object) &

     attributes([uid(c0), timepoint(1), epsilon(1)]) &

     dynamic history/2 &

     dynamic profile/1 &

     next(step(X,Y)) :-
	:prove([step(X,Y)],D) &

     pr(X) :-
	:write(preference_reasoning(X)) &

     tr(X) :-
	:write(temporal_reasoning(X)) &

     gd(Goal, Delta) :-
	:prove([gd(Goal)], Delta) &

     plan(X) :-
	:write(planning(X)) &

     pre(X) :-
	:write(preconditions(X)) &

     react(Reaction,Delta) :-
	:prove([kb_react(Reaction)],Delta) &

     do(step('RE',[])) :-
	:findall(result(Reaction,Delta), computee_basic::react(Reaction,Delta), Result),
	:print_results('Reactivity', Result) &

     do(step('GI', [])) :-
	:findall(result(Goal,Delta), computee_basic::gd(Goal,Delta), Result),
	:print_results('Goal Decision', Result) &

     do(step(NAME,INPUT)) :-
	self::get(timepoint(T0)),
	assert(history(T0, step(NAME,INPUT))),
	:(T1 is T0 + 1),
	self::set(timepoint(T1)),
	:write(execute_transition(NAME,INPUT))

    }.



rule(Label, Head, Body) :- 
	kb_gd__rule(Label, Head, Body).

complement(gd(X), gd(Y)) :- 
	kb_gd__todo(X),
	kb_gd__todo(Y),
	X \= Y.
