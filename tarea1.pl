% ------------------------------------------------------------------------------
%
% CI-1441 Paradigmas computacionales
% II-2017, Prof. Alvaro de la Ossa O., Dr.rer.nat.
%
% Tarea 1
% Ricardo Apú
% Óscar Corella
%
% ------------------------------------------------------------------------------
% listap(+Objeto): verdadero si Objeto es una lista, falso si no
%    Objeto: cualquier objeto Prolog
%    ?- listap(a).
%    ?- listap([]).
%    ?- listap([a,b,c]).
listap([]) :- !.
listap([X|Y]) :- listap(Y).
% 1. Predicados para el procesamiento de árboles y conjuntos
%
% bpp(+N,+A,-S): S es el subárbol de A cuya raíz es N; S debe ser nil si el subárbol no existe
%
bpp(N,[],S):- S = nil, !.
bpp(N,[N|T],S):- format('~w ',[N]), S =[N|T], !.
bpp(N,[H|T],S):- listap(H), bpp(N,H,S),!.
bpp(N,[H|T],S):- format('~w ',[H]), bpp(N,T,S), !.
