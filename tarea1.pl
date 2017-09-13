% ------------------------------------------------------------------------------
%
% CI-1441 Paradigmas computacionales
% II-2017, Prof. Alvaro de la Ossa O.
%
% Tarea 1
% Ricardo Apú
% Óscar Corella B32080
%
% ------------------------------------------------------------------------------
% 1. Predicados para el procesamiento de árboles y conjuntos
%
% bpp(+N,+A,-S): S es el subárbol de A cuya raíz es N; S debe ser nil si el subárbol no existe
%
%bpp(N,[],S)!.
bpp(N,[N|T],S):- format('~w ',[N]), S = [N|T], !.
bpp(N,[H|_],S):- is_list(H), length(H, L), L > 0, bpp(N,H,S),!.
bpp(N,[H|T],S):- is_list(H), bpp(N,T,S), !.
bpp(N,[H|T],S):- not(is_list(H)), format('~w ',[H]), bpp(N,T,S), !.


primero([X|Y],X) :- !.
%bap p(+N,+A,-S): S es el subarbol de A cuya raiz es N
bap(N,[N|T],S):- format('~w ',[N]), S = [N|T], !.
bap(N,[H|T],S):- is_list(H), length(H, L), L > 0, primero(H,F), format('~w ',[F]),!,bap(N,T,S),bap(N,[_|H],S),!.
bap(N,[H|T],S):- not(is_list(H)), format('~w ',[H]), bap(N,T,S), !.


%cartesiano(+A,+B,-C)
cartesiano(A,B,C):- is_list(A), is_list(B),findall([X,Y],(member(X,A),member(Y,B)),C), !.
cartesiano(_,_,C) :- C = [].

%potencia(+C,-P) P es el conjunto de potencia de C
potencia(C, P) :- not(is_list(C)), P = [].
potencia([], []).
potencia([H|T], P) :- potencia(T,P).
potencia([H|T], [H|P]) :-  potencia(T,P).




%rota una lista circularmente
rotacircular([H|T],R):- append(T,[H],R).

%le da la vuelta a los elementos de una lista
reversa([X],[X]).
reversa([X|M],Z) :- reversa(M,S), append(S,[X],Z).

%true si los primeros elementos de dos listas son iguales
comparaHeader([H|_],[H|_]).

% encripta(+He,+Ae,+As,-Hs,-Ef): Hs es el resultado de encriptar la hilera de entrada He con un
% engranaje formado por los alfabetos de entrada (Ae) y salida (As); Ef es el estado final de la máquina
encripta(He,Ae,As,Hs,Ef):-
  reversa(As, AsR),
  deverdadEncripta(He,Ae,AsR,[],Hs,Ef), !.

deverdadEncripta([],[AeH|_],[AsH|_],HHs,Hs,Ef):-
  Ef = [AeH,AsH],
  Hs = HHs,!.

deverdadEncripta(He,Ae,As,HHs,Hs,Ef):-
	comparaHeader(He,Ae),
  [AsH|_]=As,
  append(HHs,[AsH],Hs1),
  %write(Hs1),
  [_|HeT]=He,
  deverdadEncripta(HeT,Ae,As,Hs1,Hs,Ef).

deverdadEncripta(He,Ae,As,HHs,Hs,Ef):-
	rotacircular(Ae, Rae),
  rotacircular(As, Ras),
  deverdadEncripta(He,Rae,Ras,HHs,Hs,Ef).

% decripta(+Hs,+Ae,+As,+Ef,-He), que decodifica la hilera encriptada Hs usando los alfabetos Ae y As,
% iniciando en la posición del engranaje descrita por el estado final de la máquina (Ef) cuando se encriptó
% la hilera que produjo la hilera Hs.

decripta(Hs,Ae,As,[E,S],He):-
  reversa(As, AsR),
  moverAEstadoInicial(Ae,E,AeN),
  moverAEstadoInicial(AsR,S,AsN),
  deverdadDecripta(Hs,AeN,AsN,[],He),
  !.

deverdadDecripta([],[_|_],[_|_],HHe,He):-
    He = HHe,!.

deverdadDecripta(Hs,Ae,As,HHe,He):-
	comparaHeader(Hs,As),
  [AeH|_]=Ae,
  append(HHe,[AeH],He1),
  %write(Hs1),
  [_|HsT]=Hs,
  deverdadDecripta(HsT,Ae,As,He1,He).

deverdadDecripta(Hs,Ae,As,HHe,He):-
	rotacircular(Ae, Rae),
  rotacircular(As, Ras),
  deverdadDecripta(Hs,Rae,Ras,HHe,He).

moverAEstadoInicial([H|T],H,[H|T]).
moverAEstadoInicial(L,H,LN):-
  rotacircular(L, L1),
  moverAEstadoInicial(L1,H,LN).
