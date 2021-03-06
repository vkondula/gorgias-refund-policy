% ==================================================================== %
%                          GD EXAMPLE (Give or Keep Objects)           %
% ==================================================================== %
% A policy for deciding whether to Give or Keep objects as can arise   %
% for example within the context of a resource allocation problem.     %
% ==================================================================== %

:- compile('../../lib/gorgias').
:- compile('../../ext/lpwnf').


abducible(return(Ag,Res), []) :- 
	isagent(Ag),
	isres(Res).
abducible(neg(return(Ag,Res)), []) :- 
	isagent(Ag),
	isres(Res).
isagent(Ag) :- member(Ag, [agA,agB]).
isres(Res) :- member(Res, [book123,book456]).

rule(r1(Res,Ag), give(Res,Ag), [requested(Res,Ag), have(Res)]).
rule(r2(Res,Ag), neg(give(Res,Ag)), [need(Res,_), have(Res)]).


rule(r3(Res,Ag), prefer(r2(Res,Ag), r1(Res,Ag)), [need(Res,_)]).
rule(r4(Res,ResAux,Ag), prefer(r1(Res,Ag),r2(Res,Ag)), [return(Ag,Res), alternative(Res, ResAux)]).


rule(r5(Res,ResAux,Ag), prefer(r3(Res,Ag),r4(Res,ResAux,Ag)), [urgent_need(Res)]).


rule(a1(Res,ResAux), alternative(Res,ResAux), [have(ResAux), same_type(ResAux,Res)]).
rule(a2(Res,Goal), urgent_need(Res), [need(Res,Goal), urgent(Goal)]).
rule(ic(Res,Ag), neg(return(Ag,Res)), [Ag = agA]) :- isres(Res).


rule(f0,requested(book123,agA),[]).
rule(f1,requested(book123,agB),[]).
rule(f2,have(book123),[]).
rule(f3,need(book123,exams),[]).
rule(f4,have(book456),[]).
rule(f5,alternative(book123,book456),[]).
rule(f6,same_type(book123,book456),[]).

%%% UNCOMMENT THE LINE BELOW
% rule(f6,urgent(exams),[]).




start :-
	writeln('======================================================'),
	writeln('Give or Keep'),
	writeln('======================================================'),
	test11,
	writeln('------------------------------------------------------'),
	test12,
	writeln('------------------------------------------------------'),
	test21,
	writeln('------------------------------------------------------'),
	test22.

test11 :-
	Query = [give(book123,agA)],
	writeln('Test 1.1'),
	writeln(''),
	writeln('  Query:'),
	pretty(Query),
	prove(Query,Delta),
	writeln('  Delta:'),
	pretty(Delta).

test11.

test12 :-
	Query = [neg(give(book123,agA))],
	writeln('Test 1.2'),
	writeln(''),
	writeln('  Query:'),
	pretty(Query),
	prove(Query,Delta),
	writeln('  Delta:'),
	pretty(Delta).

test12.

test21 :-
	Query = [give(book123,agB)],
	writeln('Test 2.2'),
	writeln(''),
	writeln('  Query:'),
	pretty(Query),
	prove(Query,Delta),
	writeln('  Delta:'),
	pretty(Delta).

test21.

test22 :-
	Query = [neg(give(book123,agB))],
	writeln('Test 2.2'),
	writeln(''),
	writeln('  Query:'),
	pretty(Query),
	prove(Query,Delta),
	writeln('  Delta:'),
	pretty(Delta).

test22.
