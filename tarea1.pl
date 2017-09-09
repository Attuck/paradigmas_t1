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
% 1. Predicados para el procesamiento de árboles y conjuntos
%
% bpp(+N,+A,-S): S es el subárbol de A cuya raíz es N; S debe ser nil si el subárbol no existe
%
%bpp(N,[],S)!.


bpp(N,[N|T],S):- format('~w ',[N]), S = [N|T], !.
bpp(N,[H|T],S):- is_list(H), length(H, L), L > 0, bpp(N,H,S),!.
bpp(N,[H|T],S):- is_list(H), bpp(N,T,S), !.
bpp(N,[H|T],S):- not(is_list(H)), format('~w ',[H]), bpp(N,T,S), !.
