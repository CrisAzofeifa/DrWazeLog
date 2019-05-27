:-style_check(-singleton).
iniciar():-pregunta1().



o(o(SN,SV)) --> sn(SN,_Gen,Num),sv(SV,Num);sn(SN,_Gen,Num); sv(SV, Num); yn(YN), sn(SN,_Gen,Num).

 
sn(sn(DET,N),Gen,Num) --> det(DET,Gen,Num), n(N,Gen,Num); sal(SAL,Num),n(N,Gen,Num); n(N,Gen, Num).


sv(sv(VT,SN),Num) --> vt(VT,Num), sn(SN,_Gen,_Num).
sv(sv(VI),Num) --> vi(VI,Num).


det(det(X),f,sg) --> [X], {member(X,[la,una,para])}.
det(det(X),f,pl) --> [X], {member(X,[las,unas])}.
det(det(X),m,sg) --> [X], {member(X,[el,un,en,para])}.
det(det(X),m,pl) --> [X], {member(X,[los,unos])}.

sal(sal(X), sg) --> [X] ,{member(X,[hola,buenosdias])}.

yn(yn(X)) --> [X] ,{member(X,[si,no])}.

vi(vi(ladra),sg) --> [ladra].
vi(vi(ladran),pl) --> [ladran].
vi(vi(ladran),sg) --> [estoy].
vt(vt(estoy),sg) --> [estoy].
vt(vt(voy),sg) --> [voy].
vt(vt(muerden),pl) --> [muerden].


n(n(perra),f,sg) --> [perra].
n(n(perras),f,pl) --> [perras].
n(n(X),m,sg) --> [DrWaze].
n(n(X),m,sg) --> [X], {lugares(X)}.

n(n(X),m,pl) --> [X], {member(X,[perros,huesos])}.

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


%...SERIE DE PREGUNTAS Y RESPUESTAS...
recibir_mensaje(M):-write('Usuario: '),read(Entrada), atomic_list_concat(M, ' ', Entrada), phrase(o(A),M).

pregunta1():-write('DrWaze: Bienvenido a WazeLog la mejor logica de llegar a su destino. \nPor Favor indiqueme donde se encuentra. \n'),
			 recibir_mensaje(M), respuesta1(M).

respuesta1(M):-lugares(X), miembro(X,M), pregunta2(X); 
			   write('DrWaze: No puedo entenderle, por favor indiqueme de manera correcta donde se encuentra.\n'), recibir_mensaje(M), respuesta1(M).

pregunta2(O):-write('DrWaze: Muy bien ¿Cual es su destino?\n'),
			  recibir_mensaje(M), respuesta2(M,O).

respuesta2(M,O):-lugares(X), miembro(X,M), pregunta3(O,X); 
			     write('DrWaze: No puedo entenderle, por favor indiqueme de manera correcta su destino.\n'), recibir_mensaje(M), respuesta2(M,O).

pregunta3(O,D):-write('DrWaze: ¿Tiene algun destino intermedio?\n'),
			    recibir_mensaje(M), respuesta3(O,D,M).

respuesta3(O,D,M):-miembro(si,M),lugares(X),miembro(X,M),respuestaFinal1(O,D,I);
				   miembro(si,M),lugares(X), not(miembro(X,M)), preguntarI(I), respuestaFinal1(O,D,I);
				   miembro(no,M), respuestaFinal2(O,D);
				   write('DrWaze: No puedo entenderle, intente de nuevo.\n'), pregunta3(O,D).

preguntarI(I):-write('DrWaze: ¿Cual es el destino intermedio?\n'), recibir_mensaje(M), lugares(I), miembro(I,M);
			  write('DrWaze: No puedo entenderle, intente de nuevo.\n'), preguntarI(I).

respuestaFinal1(O,D,I):-write('DrWaze: Su ruta seria: '), ruta(O,I),ruta(I,D).

respuestaFinal2(O,D):-write('DrWaze: Su ruta seria: '), ruta(O,D).

miembro(X,[X|_]).
miembro(X,[_|R]):-miembro(X,R).

:-dynamic
	rpath/2.

arco(sanjose, cartago, 20).
arco(sanjose,corralillo,22).
arco(cartago,paraiso,10).
arco(cartago,pacayas,13).
arco(cartago,sanjose,20).
arco(cartago,musgoverde,10).
arco(cartago,tresrios,8).
arco(corralillo, sanjose,22).
arco(corralillo, musgoverde,10).
arco(paraiso, cervantes,4).
arco(paraiso, cachi,10).
arco(paraiso,orosi,8).
arco(pacayas, tresrios,15).
arco(pacayas, cartago,13).
arco(pacayas,cervantes,8).
arco(musgoverde, corralillo,6).
arco(musgoverde, cartago,10).
arco(tresrios, sanjose,8).
arco(tresrios, pacayas,15).
arco(cervantes, juanvinas,5).
arco(cervantes, pacayas,8).
arco(cervantes, cachi,7).
arco(cachi,orosi,12).
arco(cachi, paraiso,10).
arco(cachi,turrialba,40).
arco(cachi,cervantes,7).
arco(orosi, paraiso,8).
arco(orosi,cachi,12).
arco(juanvinas, turrialba,4).
arco(turrialba, pacayas,18).
arco(paraiso, cervantes,4).
arco(paraiso, cachi,10).
arco(paraiso, orosi,8).

camino(Ini,Fin,Dist) :- arco(Ini,Fin,Dist).


mascorto([H|Path], Dist) :-
	rpath([H|T], D), !, Dist < D,
	retract(rpath([H|_],_)),
        assert(rpath([H|Path], Dist)).
mascorto(Path, Dist) :-
         assert(rpath(Path,Dist)).



atravesar(Inicio, Camino, Dist) :-
	camino(Inicio, T, D),
	not(memberchk(T, Camino)),
	mascorto([T,Inicio|Camino], Dist+D),
	atravesar(T,[Inicio|Camino],Dist+D).

atravesar(Inicio) :-
	retractall(rpath(_,_)),
	atravesar(Inicio,[],0).
atravesar(_).




ruta(Inicio, Fin) :-
	atravesar(Inicio),
	rpath([Fin|R], Dist)->
        reverse([Fin|R], Camino),
        Distancia is round(Dist),
        writef('Su ruta es %w y recorrerá: %w km \n',
	       [Camino, Distancia]);
	writef('No conozco la ruta de %w a %w\n', [Inicio, Fin]).


