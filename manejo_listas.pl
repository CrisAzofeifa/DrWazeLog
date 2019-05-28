%...FUNCIONES PARA EL MANEJO DE LISTAS...
:-style_check(-singleton).

concatenar([],L,L).
concatenar([C|R],L,[C|R1]):-  concatenar(R,L,R1).

agregarini(X, L, [X|L]).

eliminarini(L,L1):-
agregarini(_,L1,L).

juntar([C|R],L,F):- eliminarini(L, L1), concatenar([C|R],L1,F).

sumar(X, Y, Z):-Z is X + Y.

agregar(X, [], [X]).
agregar(X, [C|R], [C|R1]):-agregar(X, R, R1).

%Función que nos permite saber si un elemento es parte de una lista
%Recibe el valor X que va a buscar y la lista en la que lo buscará
%Cláusula 1: compara el valor con la cabeza de la lista, si no se cumple pasa a la Cláusula 2.
miembro(X,[X|_]).
%Cláusula 2: llama recursivamente a la Cláusula 1 con el valor a buscar y el Resto de la lista R.
miembro(X,[_|R]):-miembro(X,R).