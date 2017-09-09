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

%cartesiano(+A,+B,-C)
cartesiano(A,B,C):- is_list(A), is_list(B),findall([X,Y],(member(X,A),member(Y,B)),C), !.
cartesiano(A,B,C) :- C = [].



rotacircular([H|T],R):- append(T,[H],R).
    
    
reversa([X],[X]).
reversa([X|M],Z) :- reversa(M,S), append(S,[X],Z).


encripta(He,Ae,As,Hs,Ef):-
  Hs=[],
  reversa(As, AsR),
  deverdadEncripta(He,Ae,AsR,Hs,Ef).
  
deverdadEncripta([],[AeH|AeT],[AsH|AsT],Hs,Ef):- Ef = [AeH,AsH],!.

deverdadEncripta(He,Ae,As,Hs,Ef):-
	comparaHeader(He,Ae),[AsH|_]=As, append(Hs,AsH,Hs1), write(Hs1),[_|HeT]=He, deverdadEncripta(HeT,Ae,As,Hs1,Ef).
deverdadEncripta(He,Ae,As,Hs,Ef):-
	not(comparaHeader(He,Ae)), rotacircular(Ae, Rae), rotacircular(As, Ras), deverdadEncripta(He,Rae,Ras,Hs,Ef).

comparaHeader([H|_],[H|_]).