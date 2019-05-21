:-style_check(-singleton).
iniciar():-recibir_mensaje([]).
recibir_mensaje(X):-write('Bienvenido a WazeLog la mejor lógica de llegar a su destino. 
						   Por Favor indíqueme donde se encuentra.'),
					write('Usuario: '),read(Entrada), atomic_list_concat(M, ' ', Entrada), oracion(M,Entrada).

%...DEFINICION DE LOS LUGARES DEL GRAFO...
lugares(cartago).
lugares(tresrios).
lugares(sanjose).
lugares(corralillo).
lugares(musgoverde).
lugares(paraiso).
lugares(pacayas).
lugares(cervantes).
lugares(cachi).
lugares(orosi).
lugares(juanvinas).
lugares(turrialba).

%...REGLA PARA DESTINO Y ORIGEN...
origen(X):-lugares(X).
destino(X):-lugares(X).

%...DEFINICION DE LOS VECINOS DE LOS LUGARES EN EL GRAFO...
vecinos(cartago, [sanjose,tresrios,pacayas,paraiso,musgoverde]).
vecinos(tresrios, [pacayas]).
vecinos(sanjose, [cartago,corralillo]).
vecinos(corralillo, [musgoverde,sanjose]).
vecinos(musgoverde, [cartago,corralillo]).
vecinos(paraiso, [cervantes,orosi,cachi]).
vecinos(pacayas, [tresrios,cartago,cervantes]).
vecinos(cervantes, [pacayas,juanvinas,cachi]).
vecinos(cachi, [cervantes,turrialba,orosi,paraiso]).
vecinos(orosi, [paraiso,cachi]).
vecinos(juanvinas, [turrialba]).
vecinos(turrialba, [cachi]).


%...DEFINICION DE ORACION...

oracion(E0,E1):-sintagma_nominal(E0,E0).


%...DEFINICION DE LAS POSIBILIDADES DE ORACION...
esUnLugar([X|_]):-lugares(X).


%...FUNCIONES...
miembro(X,[X|_]).
miembro(X,[_|R]):-miembro(X,R).

esVecino(X,Y):-vecinos(X,P), miembro(Y,P).

%...DEFINICION DE SUSTANTIVOS...
sustantivo([drWaze|_], E1).

%...DEFINICION DE SINTAGMA NOMINAL...
sintagma_nominal(E0, [_|E1]):- enlace(E0,E1),esUnLugar(E1), write('es nominal'); esUnLugar(E0),write('Ok').

%...DEFINICION DE ENLACES...
enlace([En|_],Y).

