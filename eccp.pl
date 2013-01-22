%% :- module(eccp, [
%% 		 what_knows/2,
%% 		 declare/3,
%% 		 tell/3,
%% 		 knowledgeAll/2,
%% 		 knowledge/2,
%% 		 execute/7,
%% 		 parallel/1,
%% 		 checkCondition/5,
%% 		 checkConsequence/3,
%% 		 ask/3
%% 		 ]).

:- use_module(library(clpfd)).

/** <module> K-Stores 


This work introduces an interpreter for for these extensions we call k-stores. The interpreter is a Prolog implementation of the operational semantics of the languages allowing the programmer to simulate distributed information systems. The main feature consists of an
implementation of a spatial (distributed) store that allows epistemic information in it. The system supports the specification of (named)
processes along with the ccp classic primitives, namely, ask and tell operations. The declarative view of processes is inherited from the
ccp extensions.  The orthogonal implementation of the local space abstraction and the epistemic constraint system makes further
extensions possible. Special attention is paid to the representation of distributed knowledge and common knowledge.

Download the source file here: http://cic.javerianacali.edu.co:/~anfelbar/Sitio_web/teccpf/source.tar.gz


---++ Background

Spatial and Epistemic ccp extend Concurrent Constraint Programming calculus in order to capture some notions of spatiality and knowledge that have not been addressed by other extensions. It is worth noting that both calculi, whose mathematical-theoretical foundations are currently under development are not part of the contribution of the present work. Rather, we use the underlying model to build the programming environment. We shall begin with the definition of the process construction terms. 

Let P and Q be two spatial or epistemic ccp processes, we define the language of construction terms as:

==
P,Q :=    0                 skip
          tell(c)           adds partial information
          ask(c) -> P       ask whether c, then executes P
          P paralel Q       parallel execution
          [P]_i             local scope
          {K_{i}(P)         agent i knows the result of P
==

---+++ Operational semantics 

The operational semantics defines the transformation of spatial-epistemic ccp processes, i.e., rules for the reduction of processes specified in the language. For instance, the parallel rule allows two processes to be executed concurrently (at the same logical time). 
In what follows, s_i(c) is a function representing  agent i knowledge of c, and c^i represents what agent i knows when c holds: c^i=\bigsqcup\{d\;|\;s_i(d)\sqsubseteq c\}. 

[[design/operational.jpg]]

Individual knowledge is also referred to as first order knowledge, i.e., atomic and monotonic knowledge that is not part of an inference nor interactive process. Higher order knowledge refers to the knowledge that is not explicit in the predicates but that is somehow built from them. For example, if we have a set of agents G={a_1,...,a_n} and each of them knows some predicate phi, then we can denote by E_G\phi the event that ``everybody in group G knows phi''.
Common knowledge can be defined as ``everybody knows that [k times] the event phi is the case'' (i.e. K^k(phi), for arbitrary k). The additional rule for epistemic ccp operational semantics is

[[design/operational2.jpg]]

---++ Interpreter Design

The core characteristic of the epistemic ccp calculus is the partitioned or distributed store. As the store is the shared communication medium of ccp it allows different processes to interact by
narrowing variable domains. Now, with a partitioned store processes (agents) are only aware of some partial information within the store, we say that agents have a limited knowledge of the state of affairs (i.e., variable domains states). Our programming framework should implement different abstractions needed for the proper implementation of the operational semantics of espistemic ccp; partitioned 
store, constraint imposition and domain pruning, epistemic axiomatic rules consistency, along with the tell and ask operations. 

[[design/design.jpg]]

---+++ Distributed store

A distributed store is just a projection of the knowledge of all agents over the variable domains. Any given agent is aware of a subset of all variables V of the overall store. Furthermore, agents are allowed to know a limited set of constraints over the variables of which they are aware. This projection is the realization of the K_i modal operator of the logic. 

A reflection mechanism is in charge of the communication among
agents. Note that a given agent can allow another agent to access its store and post constraints over a subset of variables, opening a communication channel. This means that one agent can ask for a particular property to hold on another
local store.

Constraint posting in local stores is the same as in classic ccp. Yet, we have a fundamental difference; any
given agent may have some knowledge (beliefs) about other agents' knowledge. The core design is nested stores. Any given agent i is waiting for its variable assignment theta_i and he may use whatever knowledge he has about other agents to do so. If the agent has knowledge about another agent, say j, then i has in his store another store representing the knowledge of agent j. Then, agent i may ask and tell partial information to their own local representation of j's knowledge. Moreover, agent j's local store may have some belief about another agent, say $l$, in which case we have nested stores working together to find the assignment theta_i for agent i.

                               | [[design/storeview2.jpg]] | [[design/storeview3.jpg]]  |

Constraint programming is concurrent in the sense that processes are executed in an interleaved fashion. Execution of processes implies ask and tell operations that possibly will narrow variable domains which in turn may unblock some other ask operations. The general mechanism is shown next.

[[design/flow1.jpg]]


---++ Getting started

This tool allows parallel execution of (named) processes. A process consist of a identifier (agent name) and a list of operations. The operations are tell and/or ask. A tell is done with the atom tell and the constraint (firts order formular), e.g. tell(X #> 10). An ask operation is done with the atom knowlegde/knowledeAll and a variable to store the current knowledge, e.g. K=knowledgeAll.
	  
---+++ Sintax

==
<program>                 ::= parallel(<list_of_processes>)       
<list_of_processes>       ::= store(<Id>, <lis_of_operations>), <list_of_processes>* |
                               everyone(<list_of_constraints>)            
<list_of_operations>      ::= tell(<Id>, <Constraint>) {,<list_operations>} | Var=knowledge(<Id>) {,<list_operations>} | 
                              Var=knoledgeAll(<Id>) {,<list_operations>} | solve(Vars) {,<list_operations>}
<list of constraints>     ::= Constraint  {,<Constraint>}
<Constraint>              ::= <Var> <Operator> <Var> 
                              sum(<list_of_vars>, <Operator>, <list_of_vars>) | all_distinct(<lis_of_vars>)
<Id>                      :: any integer or atom as prolog understands it
<Operator>                ::= #=| #\= | < | =< | > | >= 

<list_of_var>             ::= <Var> {,<Var>}
<Var>                     ::= X | Y | Z
==

An agent consists of an identifier (agent name) and a list of operations which may be tell and ask operations and another agent's specifications (fresh nested stores). Two atoms are avaliable for the creation of a new named agent. The first atom is *spatial* which creates a spatial ccp agent. The second atom is *epistemic*, and defines a new epistemic ccp agent in the program. These agent definitions are exclusive, i.e., combining them in a program specification is not permitted. All Ids should be pairwise distinct in order to avoid ambiguity. Additionally, we include a construction for making knowledge known to every agent.  We call this construction *everyone*.

If an agent knows a constraint variable he must declare within a given domain. He may do so by declaring the variable global or local to his own store. Declaring a global variable represents opening a public channel where other agents may ask and tell partial information. To declare a global variable the agent uses the *declare* atom along with the variable and its domain. If the variable is supposed to be local, then the atom *tell* is enough to make the declaration. 

A tell operation is done with the atom *tell* along with a constraint (e.g. *tell*(X #> 10)). Used without an identifier it means a tell in the owned store. Conversely, a tell with an identifier adds partial information to the store of the agent represented by the
identifier (provided an open channel between the two agents exists). For nested stores, there is no need for an opened channel, as expected.

An agent is allowed to ask whether certain formulae are entailed by the store and then apply another constraint given the response. The first way to do this is by asking in any store (with an open channel) whether a formula is entailed and then applying a constraint c in the owned store (using the atom *askI*). The second possibility is to
ask in any store (with an open channel) whether a formula is entailed and then apply a constraint c in the store in which the query is made (using the atom *ask*). This abstraction enables any given agent to a) change his current store provided some property holds or change his belief about other agents in his nested stores, and b) to change the current knowledge of other agents by means of partial information posting (as it is done in social networks and amorphous computing).

Also, another non-formal operation is allowed for agents, *solve*, which invokes search from the contraint logic programming module of prolog. This last primitive is useful when solving puzzles. If used, it should be put in the last position of the operation list. 

---+++ Constraints

Tell operations are used when an agent wants to add partial information to any given store. Partial information is represented by first order formulae.
For a practical development we use the swi-prolog constraint system, hence we interface the set of formulae they provide

The basic functions used by the programer are the telling and asking functions. The asking operations return the current knowledge of an agent. The telling functions adds partial information to the store of a particular agent. Partial information should by represented as first order formulas. The basic formulas are:

         |*Tell*|*Ask*|
         | X #= Y|knowledge|
	 | X #\= Y|knowledgeAll|
	 | X #< Y| |
	 | X #=< Y| |
	 | X #> Y| |
	 | X #>= Y| |
	 | all_distinct(Xs)| |
	 | sum(Xs, Op, Ys)| |
	 | solve(Xs)| |

Where X and Y are finite domain variables, Xs is a list of finite domain variables and Op is an operator (#=, #\=, #<, #=<, #>, #>=).

---+++ Examples

We present a brief example of a program specification. "The logicians Alice and Bob are sitting in their windowless office wondering whether or not it is raining outside. Now, none of them actually knows, but Alice knows something about her friend Carol, namely that Carol wears her red coat only if it is raining. Bob does not know this, but he just saw Carol, and noticed that she was wearing her red coat."

Our program specification uses three variables to represent the problem, namely, Rain, Jacket and Wearing. The domain for the three variables is $\{0,1\}$. Zero means that the fact is not happening, one otherwise.

==
parallel([
       store(alice, [declare(Rain in 0..1), declare(Jacket in 0..1), 
         tell(Jacket #= 1), R=knowledgeAll, print(tom, [R])]),
       store(bob, [tell(Rain in 0..1), tell(Wearing in 0..1), 
         tell(Wearing #= 1), R=knowledgeAll, print(jerry, [R])])
     ]).
==

The first attempt to know whether is raining or not fails. Neither agent is able to find out that fact. The output of that specification is Rain in {0..1} and Jacket equal to 1 for Alice. For Bob the output is Rain in {0,1} and Wearing equal to 1. The next case implies agent interaction-


==
parallel([
  store(alice, [tell(Rain in 0..1), tell(Jacket in 0..1), tell(Wearing in 0..1), 
    tell(Jacket #= 1), askI(bob, Wearing#=1 -> Rain #=1),
    R=knowledgeAll, print(alice, [R]),
    store(bob, [tell(Rain in 0..1), declare(Wearing in 0..1), 
      tell(Wearing #= 1), R=knowledgeAll, print(bob, [R])])
  ]).
==

It is easy to note that once Alice asks Bob about him seeing Carol wearing the jacket, she will be able to infer that it is actually raining. The output for Alice in this case is Rain equal to 1, Jacket equal to 1 and Wearing equal to 1. For Bob the solution is the same as that in the first attempt. This is due to the isolation of each agent's store.

The third example shows an agent solving the SEND+MORE=MONEY puzzle. All constraints are added to the store of the agents. At the end of the operations we find an atom called *solve* along with some variables. This formula makes a call to the underlying solver of *clpfd*, givin as result the solucion of the puzzle. With out this predicate we only have a basic narrowing of domains but not search.

==
parallel([
        spatial(katherine, [declare([S,E,N,D,M,O,R,Y] ins 0..9),
	       tell(sum([S*1000, E*100, N*10, D*1, M*1000, O*100, R*10, E*1], #=, [M*10000, O*1000, N*100, E*10, Y*1])),
               tell( M #\= 0), tell(S #\= 0),
	       tell(all_different([S,E,N,D,M,O,R,Y])),
	       tell(solve([S,E,N,D,M,O,R,Y]))
])]).
==

---++ Function documentation

*/

:- consult('affairs.pl').
:- dynamic tell/3.
:- dynamic tell/4.
:- dynamic declare/3.
:- dynamic checkCondition/5.
:- dynamic checkConsequence/3.
:- dynamic knowledgeAll/2.
:- dynamic knowledge/2.
:- dynamic ask/3.

/**--------------------------------
 Some nice auxiliar functions ...
----------------------------------*/
allVars([]).
allVars([H|T]):-
	var(H),
	allVars(T).
isList(Var,Result):-
	is_list(Var),
	Result=true.
isList(Var,Result):-
	not(is_list(Var)),
	Result=false.


allFDVars([]).
allFDVars([H|T]):- !,
	fd_var(H),
	allFDVars(T).

allFDorVars([]).
allFDorVars([H|T]):- !,
	(
	fd_var(H), fd_dom(H, D), write(dominio(D)), nl;
	var(H), write(variable(H)), nl;
	integer(H), write(integer(H)), nl
	),
	allFDorVars(T).

removeFD(_, []).
removeFD(Agent, [H|T]):-
	fd_dom(H,D1), retract(know(Agent, H in D1)), removeFD(Agent, T);
	removeFD(Agent, T).
assignFD(_, []).
assignFD(Agent, [H|T]):-
	fd_dom(H, D1), assert(know(Agent, H in D1)), assignFD(Agent, T).

unir(everyone(_), [], _):- !.
unir(everyone(Knowledge), [spatial(N, Goals)|T], New_store_queue):-
	append([tell(Knowledge)], Goals , Goals2),
	append([spatial(N, Goals2)], Res, New_store_queue),
	unir(everyone(Knowledge), T, Res).
unir(everyone(Knowledge), [epistemic(N, Goals)|T], New_store_queue):-
	append([tell(Knowledge)], Goals , Goals2),
	append([epistemic(N, Goals2)], Res, New_store_queue),
	unir(everyone(Knowledge), T, Res).
unir([], T, New_store_queue):- !,
	New_store_queue = T.
unir([H|T1], T, New_store_queue):-
	unir(everyone(H), T, Queue),
	unir(T1, Queue, New_store_queue).

save:-
	tell('affairs.pl'),
        listing(know),
	listing(an_agent),
	listing(implies),
	listing(everybody),
	told.
	

/*---------------------------------
 Mechanism for update the knowledge of agents when
 a variable domain changes.
----------------------------------*/	
updateFD(_, []):- !.
updateFD(Agent, [Var*_|T]):-
	fd_dom(Var,D), write(dominio(D)), nl,
	assignFD(Agent, [Var]),
	updateFD(Agent, T).

deleteFD(_, []):- !.
deleteFD(Agent, [Var*_|T]):-
	fd_dom(Var,D), write(dominio(D)), nl,
	removeFD(Agent, [Var]),
	deleteFD(Agent, T).

/*---------------------------------
  Mechanism for reflexion writing...
---------------------------------*/
isIn(_, []):- !,
	fail.
isIn(X, [(Var, _, _)|T]):-
	%write(mierda10), nl,
	X==Var, !;
	isIn(X, T), !.
	

alreadyIn(Id, [X,Domain], [], Sol):- !,
	%write(mierda9), nl,
	duplicate_term(X, Copy),
	Sol = [global(Id, [(X, Copy, Domain)])].

alreadyIn(Id, [X,Domain], [global(Agent, Variables)|T], Sol):-
	%write(mierda8), nl,
	Id == Agent, !,
	(
	    isIn(X, Variables),
	    append([global(Agent, Variables)], T, Sol), !;
	    %adding(X, Variables, NewVars),
	    duplicate_term(X, Copy),
	    append(Variables, [(X, Copy, Domain)], NewVars), !,
	    append([global(Agent, NewVars)], T, Sol)
	);
	alreadyIn(Id, [X,Domain], T, Res), !,
	append([global(Agent, Variables)], Res, Sol).

/*-----------------------------------------
   Get global variables for relfexion mechanism
-------------------------------------------*/
getVars(Id, declare(X in Domain), Current, Result):- !,
	%write(mierda7), nl,
	alreadyIn(Id, [X, Domain], Current, Sol),
	Result = Sol.
getVars(Id, declare([H|T] ins Domain), Current, Result):- !,
	%write(mierda6), nl,
	getVars(Id, declare(H in Domain), Current, Res1),
	getVars(Id, declare(T ins Domain), Res1, Result).
getVars(_, _, Current, Result):- !,
	%write(mierda5), nl,
	Result=Current.
/*-----------------------------------------
   Get local variables for an agent...
-------------------------------------------*/

getVarsOwn(Id, Var=knowledgeAll, Current, Result):- !,
	alreadyIn(Id, [Var, null], Current, Sol),
	Result = Sol.
getVarsOwn(Id, tell(X in Domain), Current, Result):- !,
	%write(mierda7), nl,
	alreadyIn(Id, [X, Domain], Current, Sol),
	Result = Sol.
getVarsOwn(Id, tell([H|T] ins Domain), Current, Result):- !,
	%write(mierda6), nl,
	getVarsOwn(Id, tell(H in Domain), Current, Res1),
	getVarsOwn(Id, tell(T ins Domain), Res1, Result).
getVarsOwn(_, _, Current, Result):- !,
	%write(mierda5), nl,
	Result=Current.
	

reflect(_, [], Current, Res):- !,
	Res = Current.
reflect(Id, [H|T], Current, Res):- !,
	getVars(Id, H, Current, Result),
	reflect(Id, T, Result, Res).

reflectOwn(_, [], Current, Res):- !,
	Res = Current.
reflectOwn(Id, [H|T], Current, Res):- !,
	getVarsOwn(Id, H, Current, Result),
	reflectOwn(Id, T, Result, Res).
	

generarReflexion(_, [], Nested, Reflexion,_):- !,
	Reflexion = Nested.

generarReflexion(Id, [global(Agent, Variables, Locals)|T], Nested, Reflexion, Father):- !,
	append([global(Agent, Variables, Locals)], Salida, Reflexion),
	generarReflexion(Id, T, Nested, Salida, Father).
generarReflexion(Id, [global(Agent, Variables)|T], Nested, Reflexion, Father):- !,
	%write(agents(Id, Agent, Father)), nl,
	(
	    Id == Agent, 
	    atom_concat(Father, Id, R),
	    append([global(R, Variables, Nested)], T, Reflexion);
	    append([global(Agent, Variables)], Salida, Reflexion),
	    generarReflexion(Id, T, Nested, Salida, Father)
	).
		

createReflexion([], Current, Reflexion, _):- !,
	Reflexion = Current.
createReflexion([spatial(Id, Goals)|T], Current, Reflexion, Father):- !,
	reflect(Id, Goals, [], Res1),
	append(Current, Res1, Res),
	atom_concat(Father, Id, Path),
	(
	    Res1==[],
	    append([global(Id,[ (null,null,null)])], Res, Salida),
	    createReflexion(T, Salida, Res2, Father),
	    createReflexion(Goals, [], Solu, Path);
	    createReflexion(T, Res, Res2, Father),
	    createReflexion(Goals, [], Solu, Path)
	),
	generarReflexion(Id, Res2, Solu, Reflexion, Father).
createReflexion([epistemic(Id, Goals)|T], Current, Reflexion, Father):- !,
	reflect(Id, Goals, [], Res1),
	append(Current, Res1, Res),
	atom_concat(Father, Id, Path),
	(
	    Res1==[],
	    append([global(Id,[ (null,null,null)])], Res, Salida),
	    createReflexion(T, Salida, Res2, Father),
	    createReflexion(Goals, [], Solu, Path);
	    createReflexion(T, Res, Res2, Father),
	    createReflexion(Goals, [], Solu, Path)
	),
	generarReflexion(Id, Res2, Solu, Reflexion, Father).

createReflexion([_|T], Current, Reflexion, Father):-
	createReflexion(T, Current, Reflexion, Father).

createMyOwn([], Current, MyOwn, _):- !,
	MyOwn = Current.
createMyOwn([spatial(Id, Goals)|T], Current, MyOwn, Father):- !,
	reflectOwn(Id, Goals, [], Res1),
	append(Current, Res1, Res),
	atom_concat(Father, Id, Path),
	(
	    Res1==[],
	    append([global(Id,[ (null,null,null)])], Res, Salida),
	    createMyOwn(T, Salida, Res2, Father),
	    createMyOwn(Goals, [], Solu, Path);
	    createMyOwn(T, Res, Res2, Father),
	    createMyOwn(Goals, [], Solu, Path)
	),
	generarReflexion(Id, Res2, Solu, MyOwn, Father).
createMyOwn([epistemic(Id, Goals)|T], Current, MyOwn, Father):- !,
	reflectOwn(Id, Goals, [], Res1),
	append(Current, Res1, Res),
	atom_concat(Father, Id, Path),
	(
	    Res1==[],
	    append([global(Id,[ (null,null,null)])], Res, Salida),
	    createMyOwn(T, Salida, Res2, Father),
	    createMyOwn(Goals, [], Solu, Path);
	    createMyOwn(T, Res, Res2, Father),
	    createMyOwn(Goals, [], Solu, Path)
	),
	generarReflexion(Id, Res2, Solu, MyOwn, Father).

createMyOwn([_|T], Current, MyOwn, Father):-
	createMyOwn(T, Current, MyOwn, Father).

/*-----------------------------
      Mechanism for reflexion reading
-------------------------------*/
/*
 With next functions we get local variables of an agent in a given space.
 Modify this to include nested variables.
*/
getReflexion(_, [], _, _):- 
	!, fail.
getReflexion(Agent, [global(Id, Variables)|T], Local, Father):- !,
	(
	    Agent == Id, Local = Variables, !;	    
	    getReflexion(Agent, T, Local, Father), !
	).
getReflexion(Agent, [global(Id, Variables, Nested)|T], Local, Father):- !,
	(
	    Agent == Id, Local = Variables, !;
	    getReflexion(Agent, Nested, Local, Father), !;
	    getReflexion(Agent, T, Local, Father), !
	).

returnCopy([], _, _):- 
	!, fail.
returnCopy([(Origin, Copy, _)|T], Var, X):- !,
	(
	    Origin == Var, X=Copy, !;
	    returnCopy(T, Var, X), !
	).
	
getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father):- !,
	(
	    getReflexion(Agent, Reflexion, Local, Father), returnCopy(Local, Var, X), !;
	    getReflexion(Agent, MyOwn, Own, Father), returnCopy(Own, Var, X), !;
	    fail
	).

getLocalVars(_, _, [], NewVars, _):- !, NewVars = [].
getLocalVars(Agent, [Reflexion, MyOwn], [H|T], NewVars, Father):- !,
	getLocalVar(Agent, [Reflexion, MyOwn], H, Var, Father),
	getLocalVars(Agent, [Reflexion, MyOwn], T, Res, Father),
	append([Var], Res, Result),
	NewVars = Result.

print(Agent, Vars, Atom, Reflexion, Father):- !,
	%write(flag00(Vars)), nl,
	getLocalVars(Agent, Reflexion, Vars, Result, Father),
	%write(flag00(Vars)), nl,
	format('    output --> ~a', Atom),
	format('(~w)~n', [Result]).


/*-----------------------------------------
   Parsing Var*Integer, Var+Integer, Var-Integer
-----------------------------------------*/

pormenos1([], Res):- !, Res = [].
pormenos1([H|T], Sol):-!,
	S is H*(-1),
	pormenos1(T, Res),
	append([S], Res, Sol).

form1(Agent, Reflexion, Var*_, Res, Father):- !,
	(
	 getLocalVar(Agent, Reflexion, Var, X, Father),
	 Res=[X];
	 Res=[Var]
	).

form1(Agent, Reflexion, Var+Value, Res, Father):- !,
	%write(form2), nl,
	getLocalVar(Agent, Reflexion, Var, Var2, Father),
	NewVar #= Var2+Value,
	Res = [NewVar].

form1(Agent, Reflexion, Var-Value, Res, Father):- !,
	%(fd_var(Var); integer(Var)), integer(Value),
	%write(form3), nl,
	getLocalVar(Agent, Reflexion, Var, Var2, Father),
	NewVar #= Var2-Value,
	Res = [NewVar].



parce(_, _, [], Res,_):- Res = [], !.
parce(Agent, Reflexion, [H|T], Res, Father):- !,
	%write(parce0(Reflexion)), nl,
	form1(Agent, Reflexion, H, R1, Father),
	%write(parce1(Reflexion)), nl,
	parce(Agent, Reflexion, T, Res2, Father),
	append(R1, Res2, Result),
	Res = Result.

form2(_*Value, Res):- !,
	Res=[Value].

parceValues([], Res):- Res = [], !.
parceValues([H|T], Res):-
	form2(H, R1),
	parceValues(T, R2),
	append(R1, R2, Res).


/*---------------------------------------
  Mechanism for formating parallel inputs.
-----------------------------------------*/
startRoots([], Salida):-!,
	Salida=[].
startRoots([spatial(Id, Goals)|T], Retorno):- !,
	append([spatial(Id, Goals, 'nested:')], Salida, Retorno),
	startRoots(T, Salida).
startRoots([epistemic(Id, Goals)|T], Retorno):- !,
	append([epistemic(Id, Goals, 'nested:')], Salida, Retorno),
	startRoots(T, Salida).

getNestedRef(Agent, [global(Id, _, Nested)|T], NestedReflexion):- !,
	(Agent==Id,
	NestedReflexion=Nested;
	getNestedRef(Agent, T, NestedReflexion)).
getNestedMyOwn(Agent, [global(Id, _, Nested)|T], NestedMyOwn):- !,
	(Agent==Id,
	NestedMyOwn=Nested;
	getNestedMyOwn(Agent, T, NestedMyOwn)).

printAll([], _, _).
printAll([spatial(Id, Goals)|T], [Reflexion, MyOwn], Father):-!,
	atom_concat(Father, Id, Path),
	(
	    Father == 'nested:',
	    format('    Knowlegde for agent ~a --> {', Id),
	    adjustKnowledge(Path, Reflexion, Path),
	    adjustKnowledge(Path, MyOwn, Path),
	    format('ø}'),
	    nl,
	    printAll(Goals, [Reflexion, MyOwn], Path);
	    getNestedRef(Father, Reflexion, NestedReflexion),
	    getNestedMyOwn(Father, MyOwn, NestedMyOwn),
	    format('    Knowlegde for ~w\'s representation of ~w --> {', [Father, Id]),
	    adjustKnowledge(Path, NestedReflexion, Father),
	    adjustKnowledge(Path, NestedMyOwn, Father),
	    format('ø}'),
	    nl,
	    printAll(Goals, [NestedReflexion, NestedMyOwn], Path)
	),
	printAll(T, [Reflexion, MyOwn], Father).
printAll([epistemic(Id, Goals)|T], [Reflexion, MyOwn], Father):-!,
	atom_concat(Father, Id, Path),
	(
	    Father == 'nested:',
	    format('    Knowlegde for agent ~a --> {', Id),
	    adjustKnowledge(Path, Reflexion, Path),
	    adjustKnowledge(Path, MyOwn, Path),
	    format('ø}'),
	    nl,
	    printAll(Goals, [Reflexion, MyOwn], Path);
	    getNestedRef(Father, Reflexion, NestedReflexion),
	    getNestedMyOwn(Father, MyOwn, NestedMyOwn),
	    format('    Knowlegde for ~w\'s representation of ~w --> {', [Father, Id]),
	    adjustKnowledge(Path, NestedReflexion, Father),
	    adjustKnowledge(Path, NestedMyOwn, Father),
	    format('ø}'),
	    nl,
	    printAll(Goals, [NestedReflexion, NestedMyOwn], Path)
	),
	printAll(T, [Reflexion, MyOwn], Father).
printAll([_|T], Reflexion, Father):- !,
	printAll(T, Reflexion, Father).

/*---------------------------------
   Mechanism for updating agent knowledge with axiomatic system rules
----------------------------------*/
updateKnowledge(_, []):- !.
updateKnowledge(Agent, [(_, Copy, Domain)|T]):- !,
	(
	    integer(Copy), %write(soyentero(Copy)), nl,
	    (
		Copy==Domain;
		Copy\==Domain, write(Copy)	
	    ),
	    write(','),
	    updateKnowledge(Agent, T);
	    fd_var(Copy), !, fd_dom(Copy, Dom), write(Dom), write(','),
	    (
		Domain==Dom;
		Domain\==Dom
		%retract(know(Agent, Copy in Domain)), assert(know(Agent, Copy in Dom))
	    ),
	    %write(knowledgeAgent(Agent, Copy)), nl,

	    updateKnowledge(Agent, T);
	    Domain == null, %write(soyNULL(Copy)), nl,
	    %write(knowledgeAgent(Agent, Copy)), nl,
	    updateKnowledge(Agent, T)
	    
	).


adjustKnowledge(Agent, Reflexion, Father):- !,
	(
	    getReflexion(Agent, Reflexion, Local, Father),
	    %write(locale(Local)), nl,
	    updateKnowledge(Agent, Local);
	    true
	).
	

	
/*----------------------------------------------------------------*/
	
/*

% is_an_agent(+Agent:agent) is det.
% 
%
%  Checks whether the Agent is an agent of the system or...
%  returns an agent of the system.
%
% @param Agent is an agent in the program
is_an_agent(Agent):-
	an_agent(Agent).*/

/* ----------------------------------------------
  Axiomatic system of the epstemic logic S4...
----------------------------------------------*/
	
% implication(+Knowledge1:predicate, +Knowledge2:predicate) is det.
%
%
%  When one thing implies another
%  Returns implication of Knowledge1
%
% @param Knowledge is whatever predicate (e.g rain implies could)
implication(Known1, Known2):-
	implies(Known1, Known2).


%% what_knows(+Agent:agent, -Knowledge:predicate) is det.
%%
%
% What is the knowledge of an agent?
% What an agent knows in the current moment.
% This can be the result of diffent events as
% a first order predicate, implication, etc.
%
% @param Agent an agent in the program
% @param Knowledge a predicate
what_knows(Agent, Knowledge):-
	know(Agent, Knowledge).

/*
%%% If I know that other agent, or myself, knows some F,
%%% then I must know that F
%% what_knows(Agent, Knowledge):-
%% 	KK=know(Agent2, Knowledge),
%% 	know(Agent, KK),
%% 	is_an_agent(Agent),
%%	is_an_agent(Agent2).

%%% This reflects the fact that "I know what I know".
%what_knows(Agent, Knowledge):-
%	Knowledge=know(Agent, KK),
%	know(Agent, KK),
%	is_an_agent(Agent).

%what_knows(Agent, Knowledge):-
% 	Knowledge=know(Agent, KK),
% 	what_knows(Agent, KK),
% 	is_an_agent(Agent).

%% what_knows(Agent2, Knowledge):-
%% 	know(Agent1, KK),
%% 	Agent2\==Agent1,
%% 	KK=know(Agent2, Knowledge),
%% 	is_an_agent(Agent1),
%% 	is_an_agent(Agent2).

%%%%%%%%% Recursively knowledge!!! %%%
%% what_knows(Agent, Knowledge):-
%% 	KK=know(Agent, Knowledge),
%% 	what_knows(Agent, KK),
%% 	is_an_agent(Agent).

%% what_knows(Agent, Knowledge):-
%% 	KK=know(Agent2, Knowledge),
%% 	what_knows(Agent, KK),
%% 	is_an_agent(Agent),
%% 	is_an_agent(Agent2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
%% When an agent knows F and F implies G,
%% then the agents also knows G.
%what_knows(Agent,Knowledge):-
%	know(Agent,MK),
%	implication(MK,Knowledge),
%	is_an_agent(Agent).

%% If I know that if I know something then I must know something else;
%% for example when you know multiply then you also know add.
%% what_knows(Agent, Known2):-
%% 	implies(Knowledge1, Knowledge2),
%% 	Knowledge1=know(Agent, Known1),
%% 	Knowledge2=know(Agent, Known2),
%% 	what_knows(Agent, Known1),
%% 	is_an_agent(Agent).

%% If I know some F about other person, then the other person knows F.
%% what_knows(Agent2, Known2):-
%% 	implies(Knowledge1, Knowledge2),
%% 	Knowledge1=know(Agent1, Known1),
%% 	Knowledge2=know(Agent2, Known2),
%% 	what_knows(Agent1, Known1),
%% 	is_an_agent(Agent1),
%% 	is_an_agent(Agent2).
*/

% true(+Knowledge:predicate) is det.
% 
%
% Some thing is true if it is known by some agent.
% All thinks known are true.
%
% @param Knowledge is either a unbound var or a value.
true(Knowledge):-
	what_knows(_,Knowledge).

/* --------------------------------------------------------------
        Basic interpreter for interleaving processes....
-----------------------------------------------------------------*/

%% declare(+Agent:agent, +X:fd_var, +Domain:fd_dom) is det.
%%
%
% Tells a variable domain to an agent. If the agent already knows the fd variable then just update its domain.
%
% ==
% Use: declare(Var in Domain). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param X a unbound or finite domain variable.
% @param Domain is a finite domain of the form l..u, see clpfd for more info.
program(Agent, declare(Var in Domain), Response, [Reflexion, MyOwn], Father):-
	getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father), 
	(
	    fd_var(X), fd_dom(X,D), retract(know(Agent, X in D)), X in Domain, fd_dom(X, D2), assert(know(Agent, X in D2)), Response=[return(done)];
	    X in Domain, assertz(know(Agent, X in Domain)), Response=[return(done)] 
	).

%% declare(+Agent:agent, +Vars:fd_var_list, +Domain:fd_dom) is det.
%%
%
% Tells an agent a list of variable domains. If the agent already knows any of those fd variables then just update its domain.
%
% ==
% Use: declare(Vars ins Domain). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param Vars a list of unbound or finite domain variables.
% @param Domain is a finite domain of the form l..u, see clpfd for more info.
program(Agent, declare(Xs ins Domain), Response, [Reflexion, MyOwn], Father):-
	getLocalVars(Agent, [Reflexion, MyOwn], Xs, Vars, Father),
	(
	    allFDVars(Vars), removeFD(Agent, Vars), Vars ins Domain, assignFD(Agent, Vars), Response=[return(done)];
	    allVars(Vars), Vars ins Domain, assignFD(Agent, Vars), Response=[return(done)]
	).

%% tell(+Agent:agent, +X:fd_var, +Domain:fd_dom) is det.
%%
%
% Tells a variable domain to an agent. If the agent already knows the fd variable then just update its domain.
%
% ==
% Use: tell(Var in Domain). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param X a unbound or finite domain variable.
% @param Domain is a finite domain of the form l..u, see clpfd for more info.
program(Agent, tell(Var in Domain), Response, [Reflexion, MyOwn], Father):-
	getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father), 
	(
	    fd_var(X), fd_dom(X,D), retract(know(Agent, X in D)), X in Domain, fd_dom(X, D2), assert(know(Agent, X in D2)), Response=[return(done)];
	    X in Domain, assertz(know(Agent, X in Domain)), Response=[return(done)]
	).


%% tell(+Agent:agent, +Vars:fd_var_list, +Domain:fd_dom) is det.
%%
%
% Tells an agent a list of variable domains. If the agent already knows any of those fd variables then just update its domain.
%
% ==
% Use: tell(Vars ins Domain). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param Vars a list of unbound or finite domain variables.
% @param Domain is a finite domain of the form l..u, see clpfd for more info.
program(Agent, tell(Xs ins Domain), Response, [Reflexion, MyOwn], Father):-
	getLocalVars(Agent, [Reflexion, MyOwn], Xs, Vars, Father),
	(
	    allFDVars(Vars), removeFD(Agent, Vars), Vars ins Domain, assignFD(Agent, Vars), Response=[return(done)];
	    allVars(Vars), Vars ins Domain, assignFD(Agent, Vars), Response=[return(done)]
	).


/* --------------------------------------------------------------
                   Asking functions...
-------------------------------------------------------------- */

%% knowledge(+Agent:agent, +Response:unbound) is det.
%%
%
% Bound Response to the firt thing known by Agent.
%
% ==
% Use: Response=knowledge. The agent is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param Response an unbound variable.
program(Agent, knowledge, Response, [Reflexion, MyOwn], Father):- !,
	adjustKnowledge(Agent, Reflexion, Father),
	adjustKnowledge(Agent, MyOwn, Father),
	what_knows(Agent, Result),
	Response=[return(Result)].

%% knowledgeAll(+Agent:agent, +Response:unbound) is det.
%%
%
% Bound Response to the entire knowledge of Agent.
%
% ==
% Use: Response=knowledgeAll. The agent is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param Response an unbound variable.
program(Agent, knowledgeAll, Response, [Reflexion, MyOwn], Father):- !,
	adjustKnowledge(Agent, Reflexion, Father), !,
	adjustKnowledge(Agent, MyOwn, Father), !,
	write(knowledgeAll), nl,
	setof(X, know(Agent,X), Result), 
	Response=[return(Result)].

%% ask(+Id:agent, +Condition:constraint, +Consequence:constraint) is det.
%%
%
% Ask in agent Id whether the condition Condition is entailed in the store. If so, add the constraint Consequence to the store of agent Id.
%
% ==
% Use: ask(Id, Condition #= Consequence). Any agent can ask any other. It will fail if the agent Id is not aware of the knowledge involve in the 
% constraints Condition and Consequence.
% ==
%
% @param Id is an agent in the program.
% @param Condition and Consequence are constraint of the form X#=Y, X#\=Y, X#<Y, X#=<Y, X#>Y, X#>=Y.
program(Agent, ask(Id, Condition -> Consequence), Response, [_, MyOwn], Father):- !,
	atom_concat(Father, Id, Path),
	checkCondition(Path, Condition, MyOwn, Res, Father), 
	(
	    Res==fail, format('Constraint ~w not applied...~n', [Consequence]);
	    checkConsequence(Path, Consequence, Res, MyOwn, Sol, Father),
	    (Sol == good, format('    Succesful ask for ~w~n', [Agent]);
		format('TELL FAILURE: The agent ~a generate inconsistency if ~w is applied... aborting...~n', [Id, Consequence])
	    )
	),
	Response=[return(done)].

%% askI(+Id:agent, +Condition:constraint, +Consequence:constraint) is det.
%%
%
% Ask in agent Id whether the condition Condition is entailed in the store. If so, add the constraint Consequence to the store of the agent invoking this
% call.
%
% ==
% Use: ask(Id, Condition #= Consequence). Any agent can ask any other. It will fail if the agent Id is not aware of the knowledge involve in the 
% constraint Condition or if the agent invoking this call has an inconsistency with the constraint Consequence.
% ==
%
% @param Id is an agent in the program.
% @param Condition and Consequence are constraint of the form X#=Y, X#\=Y, X#<Y, X#=<Y, X#>Y, X#>=Y.
program(Agent, askI(Id, Condition -> Consequence), Response, [_, MyOwn], Father):- !,
	concat_atom([H|T],':',Id),
	%write(valordeHT([H,T])), nl,
	(
	    T==[], atom_concat(Father, H, Path), !;
	    atom_concat(Agent, H, Path), !
	),
	%atom_concat(Father, Id, Path),
	%write(names([Father, Id, Path])), nl,
	%write(myown(MyOwn)), nl,
	checkCondition(Path, Condition, MyOwn, Res, Father), 
	(
	    Res==fail, format('    Constraint ~w not applied...~n', [Consequence]);
	    checkConsequence(Agent, Consequence, Res, MyOwn, Sol, Father),
	    (Sol == good, format('    Succesful ask for ~w~n', [Agent]);
		checkConsequence(Agent, Consequence, Res, MyOwn, Sol2, Father),
		(Sol2 == good, format('    Succesful ask for ~w~n', [Agent]);
		    format('TELL FAILURE: The agent ~a generate inconsistency if ~w is applied... aborting...~n', [Agent, Consequence])
		)
	    )
	),
	Response=[return(done)].



%% checkCondition(+Agent:agent, +Knowledge:predicate, +R:struct, +Response:unbound, +Father:agent) is det.
%% 
%
% Checks whether the constraint Knowledge is entailed by the store of agent Agent.
% This should not be used by the programer, it is used in the asking operations.
%
% @param Agent is an agent in the program.
% @param Knowledge is a constraint of the form X#=Y, X#\=Y, X#<Y, X#=<Y, X#>Y, X#>=Y.
% @param R is an structure with the current knowledge of all agents.
% @param Response is an unbound var that will be bound to fail if the constraint is not entailed and to a reified variable otherwise.
% @param Father is the father (agent) of the current executing agent.
checkCondition(Agent, X #= Y, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y),	Copy1 #= Y #==> Result;
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy1 #= Copy2 #==> Result;
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy2 #= X #==> Result;
	    format('ASK FAILURE: The agent ~a it not aware of ~w...~n', [Agent, X #= Y]), Result=fail
	).

checkCondition(Agent, X #\= Y, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y),	Copy1 #\= Y #==> Result;
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy1 #\= Copy2 #==> Result;
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy2 #\= X #==> Result;
	    format('ASK FAILURE: The agent ~a it not aware of ~w...~n', [Agent, X #\= Y]), Result=fail
	).

checkCondition(Agent, X #< Y, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y),	Copy1 #< Y #==> Result;
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy1 #< Copy2 #==> Result;
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy2 #< X #==> Result;
	    format('ASK FAILURE: The agent ~a it not aware of ~w...~n', [Agent, X #< Y]), Result=fail
	).

checkCondition(Agent, X #=< Y, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y),	Copy1 #=< Y #==> Result;
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy1 #=< Copy2 #==> Result;
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy2 #=< X #==> Result;
	    format('ASK FAILURE: The agent ~a it not aware of ~w...~n', [Agent, X #=< Y]), Result=fail
	).

checkCondition(Agent, X #> Y, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y),	Copy1 #> Y #==> Result;
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy1 #> Copy2 #==> Result;
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy2 #> X #==> Result;
	    format('ASK FAILURE: The agent ~a it not aware of ~w...~n', [Agent, X #> Y]), Result=fail
	).

checkCondition(Agent, X #>= Y, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y),	Copy1 #>= Y #==> Result;
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy1 #>= Copy2 #==> Result;
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Copy2 #>= X #==> Result;
	    format('ASK FAILURE: The agent ~a it not aware of ~w...~n', [Agent, X #>= Y]), Result=fail
	).


%% checkConsequence(+Agent:agent, +Knowledge:predicate, +Response:unbound) is det.
%% 
%
% Checks whether the constraint Knowledge can be applied to the store of agent Agent. If so, the constraint is applied.
% This should not be used by the programer, it is used in the asking operations.
% 
%
% @param Agent is an agent in the program.
% @param Knowledge is a constraint of the form X#=Y, X#\=Y, X#<Y, X#=<Y, X#>Y, X#>=Y.
% @param Response is an unbound var that will be bound to fail if an inconsistency is achieve or good otherwise.
checkConsequence(Agent, X #= Y, Condicion, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y), Condicion #==> Copy1 #= Y, Result=good; 
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy1 #= Copy2, Result = good; 
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy2 #= X, Result=good; 
	    Result=fail
	).

checkConsequence(Agent, X #\= Y, Condicion, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y), Condicion #==> Copy1 #\= Y, Result=good; 
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy1 #\= Copy2, Result = good; 
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy2 #\= X, Result=good; 
	    Result=fail
	).

checkConsequence(Agent, X #< Y, Condicion, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y), Condicion #==> Copy1 #< Y, Result=good; 
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy1 #< Copy2, Result = good; 
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy2 #< X, Result=good; 
	    Result=fail
	).

checkConsequence(Agent, X #=< Y, Condicion, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y), Condicion #==> Copy1 #=< Y, Result=good; 
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy1 #=< Copy2, Result = good; 
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy2 #=< X, Result=good; 
	    Result=fail
	).

checkConsequence(Agent, X #> Y, Condicion, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y), Condicion #==> Copy1 #> Y, Result=good; 
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy1 #> Copy2, Result = good; 
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy2 #> X, Result=good; 
	    Result=fail
	).

checkConsequence(Agent, X #>= Y, Condicion, Reflexion, Result, Father):- !,
	(
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), integer(Y), Condicion #==> Copy1 #>= Y, Result=good; 
	    getLocalVar(Agent, [Reflexion, []], X, Copy1, Father), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy1 #>= Copy2, Result = good; 
	    integer(X), getLocalVar(Agent, [Reflexion, []], Y, Copy2, Father), Condicion #==> Copy2 #>= X, Result=good; 
	    Result=fail
	).


/* --------------------------------------------------------------
   Telling functions... 
 -------------------------------------------------------------- */	


/* Finite domain constraints functions
 In order to avoid tautologies we remove 
 previous predicates of the variables 
 and add the same vars with new domain... */

%% tell(+Agent:agent, +X:fd_var, +Y:fd_var) is det.
%%
%
% Tells an agent that X equal to Y. One of them must be a fd_var, the other may be an integer.
%
% ==
% Use: tell(X #= Y). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param X a finite domain variable or integer.
% @param Y a finite domain variable or integer.
program(Agent, tell(Var #= Y), Response, [Reflexion, MyOwn], Father):- !,
	getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father), 
	(
	    fd_var(X), integer(Y), X #= Y, Response=[return(done)];
	    fd_var(X), fd_var(Y), X #= Y, Response=[return(done)];
	    fd_var(Y), integer(X), X #= Y, Response=[return(done)];
	    var(X), integer(Y), X #= Y, Response=[return(done)];
	    format('~t    ERROR: tell constraint(~w #= ~w) failed ...~n', [Var, Y])
	).

%% tell(+Agent:agent, +X:fd_var, +Y:fd_var) is det.
%%
%
% Tells an agent that X is different than Y. One of them must be a fd_var, the other may be an integer.
%
% ==
% Use: tell(X #\= Y). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param X a finite domain variable or integer.
% @param Y a finite domain variable or integer.
program(Agent, tell( Var #\= Y), Response, [Reflexion, MyOwn], Father):- !,
	getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father), 
	(
	    fd_var(X), fd_var(Y), X #\= Y, Response=[return(done)];
	    fd_var(X), integer(Y), X #\= Y, Response=[return(done)];
	    fd_var(Y), integer(X), X #\= Y, Response=[return(done)]
	).
	

%% tell(+Agent:agent, +X:fd_var, +Y:fd_var) is det.
%%
%
% Tells an agent that X is less than Y. One of them must be a fd_var, the other may be an integer.
%
% ==
% Use: tell(X #< Y). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param X a finite domain variable or integer.
% @param Y a finite domain variable or integer.
program(Agent, tell( Var #< Y), Response, [Reflexion, MyOwn], Father):-
	getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father), 
	(
	    fd_var(X), fd_var(Y), X #< Y, Response=[return(done)];
	    fd_var(X), integer(Y), X #< Y, Response=[return(done)];
	    fd_var(Y), integer(X), X #< Y, Response=[return(done)]
	).
%% tell(+Agent:agent, +X:fd_var, +Y:fd_var) is det.
%%
%
% Tells an agent that X is less or equal than Y. One of them must be a fd_var, the other may be an integer.
%
% ==
% Use: tell(X #=< Y). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param X a finite domain variable or integer.
% @param Y a finite domain variable or integer.
program(Agent, tell( Var #=< Y), Response, [Reflexion, MyOwn], Father):-
	getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father), 
	(
	    fd_var(X), fd_var(Y), X #=< Y, Response=[return(done)];
	    fd_var(X), integer(Y), X #=< Y, Response=[return(done)];
	    fd_var(Y), integer(X), X #=< Y, Response=[return(done)]
	).

%% tell(+Agent:agent, +X:fd_var, +Y:fd_var) is det.
%%
%
% Tells an agent that X is greater than Y. One of them must be a fd_var, the other may be an integer.
%
% ==
% Use: tell(X #> Y). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.%
% @param X a finite domain variable or integer.
% @param Y a finite domain variable or integer.
program(Agent, tell( Var #> Y), Response, [Reflexion, MyOwn], Father):-
	%write(tell(Agent,Var,Y)), nl,
	getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father),
	%fd_dom(X, D1), write(valorX(D1)), nl,
	(
	    getLocalVar(Agent, [Reflexion, MyOwn], Y, X2, Father),
	    %fd_dom(X2, D2), write(valorY(D2)), nl,
	    (
		fd_var(X), fd_var(X2),  X #> X2, Response=[return(done)];
		fd_var(X), integer(X2), X #> X2, Response=[return(done)];
		fd_var(X2), integer(X), X #> X2, Response=[return(done)]
	    );  
	    (
		fd_var(X), fd_var(Y), nl, X #> Y, Response=[return(done)];
		fd_var(X), integer(Y), X #> Y, Response=[return(done)];
		fd_var(Y), integer(X), X #> Y, Response=[return(done)]
	    )
	).

%% tell(+Agent:agent, +X:fd_var, +Y:fd_var) is det.
%%
%
% Tells an agent that X is greater or equal than Y. One of them must be a fd_var, the other may be an integer.
%
% ==
% Use: tell(X #>= Y). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param X a finite domain variable or integer.
% @param Y a finite domain variable or integer.
program(Agent, tell( Var #>= Y), Response, [Reflexion, MyOwn], Father):-
	getLocalVar(Agent, [Reflexion, MyOwn], Var, X, Father), 
	(
	    fd_var(X), fd_var(Y), X #>= Y,  Response=[return(done)];
	    fd_var(X), integer(Y), X #>= Y, Response=[return(done)];
	    fd_var(Y), integer(X), X #>= Y, Response=[return(done)]
	).

%% tell(+Agent:agent, +Xs:fd_var_lis) is det.
%%
%
% Tells an agent that all elements in Xs are pairwise distinc.
%
% ==
% Use: tell(all_different(Xs)). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param Xs a finite domain variable list.
program(Agent, tell(all_different(Xs)), Response, [Reflexion, MyOwn], Father):- 	
	getLocalVars(Agent, [Reflexion, MyOwn], Xs, Vars, Father),
	all_different(Vars), Response=[return(done)].

%% tell(+Agent:agent, +Xs:fd_var_lis, +Op:operator,Ys:fd_var_list) is det.
%%
%
% Tell the agent that sumation of all variables in Xs are related to the summation of all variables in Ys with operator Op.
%
% ==
% Use: tell(sum(Xs, Op, Ys). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param Xs a finite domain variable list, term of the form FdVar*Integer or FdVar+Integer.
% @param Op is one operator. Possibilities are #=, #\=, #<, #=<, #>, #>=.
% @param Ys a finite domain variable list, term of the form FdVar*Integer or FdVar+Integer.
program(Agent, tell( sum([L|T1], Op, [R|T2])), Response, [Reflexion, MyOwn], Father):- !,
	%write(sum0), nl,
	parce(Agent, [Reflexion, MyOwn], [L|T1], Res1, Father),
	%write(sum000), nl,
	parce(Agent, [Reflexion, MyOwn], [R|T2], Res2, Father),
	%write(sum1), nl,
	parceValues([L|T1], Values1),
	parceValues([R|T2], Values2),
	write(sum2), nl,
	scalar_product(Values1, Res1, #=, X), 
	scalar_product(Values2, Res2, #=, Y),
	write(sum3), nl,
	(
	    Op== #=, X #= Y;
	    Op== #=<, X #=< Y;
	    Op== #>, X #> Y;
	    Op== #>=, X#>= Y;
	),
	Response=[return(done)].
	

%%%%%%%%%%%%%%%%%%%
program(Agent, tell(Id, Resto), Response, [Reflexion, MyOwn], Father):-!,
	%write(nesteTell(Id)), nl,
	%atom_concat(Agent, gent, Path),
	concat_atom([H|T],':',Id),
	%write(valordeHT([H,T])), nl,
	(
	    T==[], atom_concat(Father, H, NewId), !;
	    atom_concat(Agent, H, NewId), !
	),
	%write(nestedTell(NewId, Agent)), nl,
	(
	 program(NewId, Resto, Response, [Reflexion, MyOwn], Agent);
	 %concat_atom([H|T],':',Agent),
	 format('    Constraint posting failed(~w) for agent ~w...~n', [Resto, Id])
	).


%% tell(+Agent:agent, +Xs:fd_var_lis) is det.
%%
%
% Force the agent to find the solution to the list Xs.
%
% ==
% Use: tell(solve(Xs)). The agent receiving the tell is the one invoking this call.
% ==
%
% @param Agent is an agent in the program.
% @param Xs a finite domain variable list.
program(Agent, tell(solve(Vars)), Response, [Reflexion, MyOwn], Father):-
	getLocalVars(Agent, [Reflexion, MyOwn], Vars, Varis, Father), !,
	label(Varis),  
	Response=[return(done)].

/**-------------------------------
 Simple execution of a program..
---------------------------------*/	
execute(_, _, [], N, terminated(N), _, _).
execute(Type, _, Goals, 1, continuation(Type, Goals, Father), _, Father):- !.
execute(Type, Agent, [Var = Rhs|More], Reds, Result, [Reflexion, MyOwn], Father):- !,
	getLocalVar(Agent, [Reflexion, MyOwn], Var, Copy, Father),
	execute(Type, Agent, [Rhs, Copy|More], Reds, Result, [Reflexion, MyOwn], Father).
execute(Type, Agent, [return(done)|T], Reds, Result, Reflexion, Father):- !,
	execute(Type, Agent, T, Reds, Result, Reflexion, Father).
execute(Type, Agent, [return(Value), Var|T], Reds, Result, Reflexion, Father):- !,
	Var = Value,
	Reds1 is Reds + 1,
	execute(Type, Agent, T, Reds1, Result, Reflexion, Father).
execute(Type, Agent, [print(A,X)|T], Reds, Result, [Reflexion, MyOwn], Father):- !,
	print(Agent, X, A, [Reflexion, MyOwn], Father), 
	Reds1 is Reds + 1, 
	execute(Type, Agent, T, Reds1, Result, [Reflexion, MyOwn], Father).
execute(Type, Agent, [nl|T], Reds, Result, Reflexion, Father):- !,
	nl,
	execute(Type, Agent, T, Reds, Result, Reflexion, Father).
execute(Type, Agent, [{X}|T], Reds, Result, Reflexion, Father):-
	call(X), !,
	Reds1 is Reds+1,
	execute(Type, Agent, T, Reds1, Result, Reflexion, Father).
execute(Type, Agent, [spatial(Id, Goals)|More], Reds, Result, [Reflexion, MyOwn], Father):- !,
	getNestedRef(Agent, Reflexion, NestedReflexion),
	getNestedMyOwn(Agent, MyOwn, NestedMyOwn),
	parallel(solve([spatial(Id, Goals, Agent),fin]), [NestedReflexion, NestedMyOwn]),
	execute(Type, Agent, More, Reds, Result, [Reflexion, MyOwn], Father).
execute(Type, Agent, [epistemic(Id, Goals)|More], Reds, Result, [Reflexion, MyOwn], Father):- !,
	getNestedRef(Agent, Reflexion, NestedReflexion),
	getNestedMyOwn(Agent, MyOwn, NestedMyOwn),
	parallel(solve([epistemic(Id, Goals, Agent),fin]), [NestedReflexion, NestedMyOwn]),
	execute(Type, Agent, More, Reds, Result, [Reflexion, MyOwn], Father).
%% execute(+Type:atom, +Agent:agent, +Xs:list, +Count:integer, -Response, +Reflexion, +Father:agent) is det.
%%
%
% Process an agent operations until the maximum number of executions has been performed. This should not be used by the programer, it is used in the parallel call.
%
% ==
% Do not use directly.
% ==
%
% @param Type is the type of the agent (spatial or epistemic).
% @param Agent is an agent in the program.
% @param Xs is the list of operations.
% @param N is the  number of the current execution.
% @param Response is bound to terminated(N) or continuation(Goals).
% @param Father is the father (agent) of the current executing agent.
execute(Type, Agent, [Lhs|More], Reds, Result, [Reflexion, MyOwn], Father):- !,
	(
	 program(Agent, Lhs, Rhs, [Reflexion, MyOwn], Father), !;
	 concat_atom([H|T],':',Agent),
	 format('    Constraint posting failed(~w) for agent ~w...~n', [Lhs, T])
	),
	append(Rhs, More, More1),
	Reds1 is Reds+1,
	execute(Type, Agent, More1, Reds1, Result, [Reflexion, MyOwn], Father).
	
/*------------------------------------------------------------
            Interleaved execution of programs
--------------------------------------------------------------*/

makeTellList([], Current, Result, _):- !,
	Result = Current.
makeTellList([tell(Constraint)|T], Current, Result, Father):- !,
	append(Current, [tell(Constraint)], NewCurrent),
	makeTellList(T, NewCurrent, Result, Father).
makeTellList([ask(Id, Condition -> Consequence)|T], Current, Result, Father):-
	atom_concat(Father, Id, Path),
	append(Current, [askI(Path, Condition -> Consequence)], NewCurrent),
	makeTellList(T, NewCurrent, Result, Father).
makeTellList([askI(Id, Condition -> Consequence)|T], Current, Result, Father):-
	atom_concat(Father, Id, Path),
	append(Current, [askI(Path, Condition -> Consequence)], NewCurrent),
	makeTellList(T, NewCurrent, Result, Father).
makeTellList([H|T], Current, Result, Father):- !,
	makeTellList(T, Current, Result, Father).

dropNested([], Current, Result):- !,
	Result = Current.
dropNested([epistemic(Id, Goals)|T], Current, Result):- !,
	append(Current, Salida, Result),
	dropNested(T, [], Salida).
dropNested([H|T], Current, Result):- !,
	append(Current, [H], NewCurrent),
	dropNested(T, NewCurrent, Result).
	
getAtomicTell([], Current, Result, C2, NewInput, Father):-!,
	Result = Current,
	NewInput = C2.
getAtomicTell([epistemic(Id, Goals)|T], Current, Result, C2, NewInput, Father):-!,
	makeTellList(Goals, [], List, Father),
	append(Current, List, NewList),
	atom_concat(Father, Id, Path),
	getAtomicTell(Goals, [], Retorno, [], NI2, Path),
	%write(nested(NewList)), nl,
	append(NewList, Retorno, NewCurrent),
	dropNested(Goals, [], OutNested),
	%write(drop1(Id, OutNested)), 
	append(OutNested, Retorno, NewGoals),
	append(NI2, NewGoals, TotalGoals), 
	append(C2, [epistemic(Id, TotalGoals)], C3),
	getAtomicTell(T, NewCurrent, Result, C3, NewInput, Father).
getAtomicTell([_|T], Current, Result, C2, NewInput, Father):- !, 
	getAtomicTell(T, Current, Result, C2, NewInput, Father).

getETells([], Current, Result):- !,
	Result = Current.
getETells([epistemic(Id, Goals)|T], Current, Result):- !,
	makeTellList(Goals, [], List, none),
	%write(tellGoals), nl,
	dropNested(Goals, [], OutNested),
	%write(drop2(Id, OutNested)), 
	getAtomicTell(Goals, [], NewGoals, [], NewInput, Id),
	append(OutNested, NewGoals, AllGoals),
	append(NewInput, AllGoals, TotalGoals),
	%write(getTells(Id, TotalGoals)), nl,
	append([epistemic(Id, TotalGoals)], Salida, Result),
	getETells(T, Current, Salida).

parallel([]).
parallel([everyone([H|T1])|T]):-
	unir([H|T1], T, New_store_queue),
	parallel(New_store_queue).
parallel([spatial(Id, Goals)|T]):- !,
	format('~n~t...Initiating spatial agents execution...~n~n'),
	createReflexion([spatial(Id, Goals)|T], [], Reflexion, 'nested:'),
	createMyOwn([spatial(Id, Goals)|T], [], MyOwn, 'nested:'),
	startRoots([spatial(Id, Goals)|T], Retorno),
	%write(reflexion(Reflexion)), nl,
	%write(myOwn(MyOwn)), nl,
	%write(roots(Retorno)),
	parallel(solve(Retorno), [Reflexion, MyOwn]),
	printAll([spatial(Id, Goals)|T], [Reflexion, MyOwn], 'nested:'),
	format('~n~t...Spatial agents execution done...'),
	format('~n~t...Have a nice day!...~n').
parallel([epistemic(Id, Goals)|T]):- !, 
	format('~n~t...Initiating epistemic agents execution...~n~n'),
	getETells([epistemic(Id, Goals)|T], [], Sol),
	%write(newsssss(Sol)), nl,
	createReflexion(Sol, [], Reflexion, 'nested:'),
	createMyOwn(Sol, [], MyOwn, 'nested:'),
	startRoots(Sol, Retorno),
	%write(startRoots(Retorno)), nl,
	parallel(solve(Retorno), [Reflexion, MyOwn]),
	printAll(Sol, [Reflexion, MyOwn], 'nested:').
	%format('~n~t...Epistemic agents execution done...'),
	%format('~n~t...Have a nice day!...~n').

parallel(solve([fin]), _):- !.
parallel(solve([fin|T]), Reflexion):- !, 
	append(T, [fin], Salida), 
	parallel(solve(Salida), Reflexion).
parallel(solve([]), _):- !, 
	retractall(know(_, _)).

%% parallel(+As:list) is det.
%%
%
% Process an agent operations until the maximum number of executions has been performed.
% This operation is the entry point of the interpreter.
%
% ==
% Use: parallel[store(Agent, Operations)|Xr]. The agent receiving the tell is the one invoking this call.
% Use: parallel[everyone(Operations)|T]. The agent receiving the tell is the one invoking this call.
% ==
% @param As is a list of stores of the form store(Agent's id, list of operations). 
parallel(solve([spatial(Id, Goals, Father)|T]), [Reflexion, MyOwn]):- !,
	format('    Executing agent ~w...~n', [Id]),
	atom_concat(Father, Id, Path),
	execute(spatial, Path, Goals, 0, Result, [Reflexion, MyOwn], Father),
	parallel(Result, Id, T, [Reflexion, MyOwn]).
parallel(solve([epistemic(Id, Goals, Father)|T]), [Reflexion, MyOwn]):- !,
	format('    Executing agent ~w...~n', [Id]),
	atom_concat(Father, Id, Path),
	execute(epistemic, Path, Goals, 0, Result, [Reflexion, MyOwn], Father),
	parallel(Result, Id, T, [Reflexion, MyOwn]).

parallel(terminated(_), Id, T, [Reflexion, MyOwn]):- !,
	format('    finish agent ~w~n', [Id]),
	parallel(solve(T), [Reflexion, MyOwn]).

parallel(continuation(spatial, Goals, Father), Id, Store_queue, [Reflexion, MyOwn]):- !,
	append(Store_queue, [spatial(Id, Goals, Father)], New_store_queue),
	parallel(solve(New_store_queue), [Reflexion, MyOwn]). 
parallel(continuation(epistemic, Goals, Father), Id, Store_queue, [Reflexion, MyOwn]):- !,
	append(Store_queue, [epistemic(Id, Goals, Father)], New_store_queue),
	parallel(solve(New_store_queue), [Reflexion, MyOwn]).
	

/*------------------------------------
           END OF FILE...
--------------------------------------*/

/*
* Create a stand-alone executable.
*
*/
make_program :-						%pce_autoload_all,
    check, qsave_program('k-stores.bin', 
          [autoload(true), goal(pce_main_loop(parallel)), stand_alone(true)]).

ejemplo1 :-
	parallel([
    epistemic(katherine, [tell(Y in 1..8), tell(Z in 9..15),
        epistemic(camilo, [tell(X in 0..5), tell(X #= 2), tell(Z in 0..20)]), 
        tell(Y #= 5)]),
        epistemic(andres, [tell(Y in 0..7), 
        epistemic(mauricio, [tell(Y in 1..5), tell(Y #= 4), tell(Z in 0..50)])
    ])
]).