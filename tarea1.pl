% ------------------------------------------------------------------------------
%
% CI-1441 Paradigmas computacionales
% II-2017, Prof. Alvaro de la Ossa O.
%
% Tarea 1
% Ricardo Apú C B40399
% Óscar Corella Q B32080
%
% ------------------------------------------------------------------------------
% 1. Predicados para el procesamiento de árboles y conjuntos
%
% bpp/3 (+N,+A,-S): S es el subárbol de A cuya raíz es N siendo recorrido en profundidad primero;
% N nodo raiz que buscamos
%S subarbol de A cuya raiz es N
%bpp(d,[a,b,[c,d,e],[f,[g,h],i]],X) → X = d; despliegue:a b c d


bpp(N,[N|T],S):- format('~w ',[N]), S = [N|T], !.
bpp(N,[H|_],S):- is_list(H), length(H, L), L > 0, bpp(N,H,S),!.
bpp(N,[H|T],S):- is_list(H), bpp(N,T,S), !.
bpp(N,[H|T],S):- not(is_list(H)), format('~w ',[H]), bpp(N,T,S), !.


primero([X|Y],X) :- !.


% bap/3 (+N,+A,-S): S es el subárbol de A cuya raíz es N siendo recorrido en anchura primero;
% N nodo raiz que buscamos
%S subarbol de A cuya raiz es N
%bap(d,[a,b,[c,d,e],[f,[g,h],i]],X) → X = d; despliegue:a b c f d
bap(N,L,S):-
getHeads(L,Lh),
not(member(Lh,N)),
printElements(Lh,N),
getTails(L,Lt),
bap(N,Lt,S).

bap(N,L,S):-
getHeads(L,Lh),
member(Lh,N),
printElements(Lh,N),
getTree(L,N,S).

getTree([N|T],N,N).
getTree([[N|T1]|T],N,[N|T1]).
getTree([H|T],N,S):-getTree(T,N,S).


getHeads([],[]).
getHeads([X|Xr],[Y|Yr]):-getHead(X,Y),getHeads(Xr,Yr).
getHead(X,X):-atomic(X).
getHead([X|_],X).

getTails([],[]).
getTails([X|Xr],[Y|Yr]):-getTail(X,Y),getTails(Xr,Yr).
getTail(X,_):-atomic(X).
getTail([_|X],X).

printElements([N|_],N):-write(N).
printElements([X|[]],_):-write(X).
printElements([H|T],N):-write(H),printElements(T,N).



%cartesiano/3(+A,+B,-C) : es el producto cartesiano de A y B
%A es la primera lista
%B es la segunda lista
%C es la lista con  el producto cartesiando de A y B
%validacion: A y B tienen que ser lsitas.
cartesiano(A,B,C):- is_list(A), is_list(B),findall([X,Y],(member(X,A),member(Y,B)),C), !.
cartesiano(_,_,C) :- C = [].

%potencia(+C,-P) P es el conjunto de potencia de C
%C es la lista
%P es todo el conjunto de potencias
%validacion: C debe ser una lista, en caso contrario devuelve una lista vacia.
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

% encripta(+He,+Ae,+As,-Hs,-Ef): encripta una hilera de un alfabeto de entrada, en una hilera de un alfabeto de salida
%He es la hilera de entrada a encriptar
%Hs es el resultado de encriptar la hilera de entrada He con un
%Ae es el alfabeto de entrada
%As es el alfabeto de salida
%EF es el estado final de la maquina
% ej:
%encripta([c,a,c,a],[a,b,c,d],[1,2,3,4],HS,EF).
%HS = [2, 4, 2, 4],
%EF = [a, 4].


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
%HS: Hilera que produjo encriptada
%AE: alfabeto de entrada de la hilera que fue encriptada
%AS : alfabeto de la hilera que queremos desencriptar
%EF: estado final de la encripcion
%HE: hilera resultante de desencriptar
%ej: Basandonos en el ejemplo del encriptada
%decripta([2,4,2,4],[a,b,c,d],[1,2,3,4],[a,4],HE).
%HE = [c, a, c, a].

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
