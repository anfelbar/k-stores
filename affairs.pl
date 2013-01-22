:- dynamic know/2.

%know(andres, hace_sol).
%know(andres, know(andres, codigo_katherine)).
%know(andres, know(katherine, birthday)).
%know(katherine, know(andres, hermoso)).
%know(katherine, llueve).
%know(katherine, llueve).

:- dynamic know/3.


:- dynamic an_agent/1.

%an_agent(andres).
%an_agent(katherine).

:- dynamic implies/2.

%implies(hace_sol, hace_calor).
%implies(hace_sol, hace_calor).
%implies(know(andres, katherine_lo_ama), know(katherine, andres_la_ama)).
%implies(hace_sol, hace_calor).

:- dynamic everybody/1.


