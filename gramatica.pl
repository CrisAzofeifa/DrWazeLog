%...Definición de las estructuras de oración válidas del programa...
%Clausula 1: un sintagma nominal junto con uno verbal.
%Clausula 2: un sintagma nominal solo.
%Clausula 3: solamente un sintagma verbal.
%Clausula 4: un si o un no.
%Clausula 5: un si o un no, acompañado de un sintagma nominal. 
o(o(SN,SV)) --> sn(SN,_Gen,Num),sv(SV,Num);sn(SN,_Gen,Num); sv(SV, Num); yn(YN); yn(YN), sn(SN,_Gen,Num).

%...Definición del sintagma nominal
%Clausula 1: determinante con un sustantivo
%Clausula 2: saludo con un sustantivo
%Clausula 3: solo un sustantivo.
sn(sn(DET,N),Gen,Num) --> det(DET,Gen,Num), n(N,Gen,Num); sal(SAL,Num),n(N,Gen,Num); n(N,Gen, Num).


%Definición del sintagma verbal:
%Clausula 1: un verbo junto con un sintagma nominal
%Clausula 2: solamente un verbo.
sv(sv(VT,SN),Num) --> vt(VT,Num), sn(SN,_Gen,_Num).
sv(sv(VI),Num) --> vi(VI,Num).


%Definición de los determinantes válidos para las oraciones.
det(det(X),f,sg) --> [X], {member(X,[la,una,para])}.
det(det(X),f,pl) --> [X], {member(X,[las,unas])}.
det(det(X),m,sg) --> [X], {member(X,[el,un,en,para])}.
det(det(X),m,pl) --> [X], {member(X,[los,unos])}.

%Definición de saludos válidos para las oraciones.
sal(sal(X), sg) --> [X] ,{member(X,[hola,buenosdias])}.

%Definición del si y el no para las oraciones
yn(yn(X)) --> [X] ,{member(X,[si,no])}.

%Definición de los verbos válidos para las oraciones junto, además se indica si son singulares o plurales.
vi(vi(estoy),sg) --> [estoy].
vt(vt(estoy),sg) --> [estoy].
vt(vt(voy),sg) --> [voy].
vt(vt(vamos),pl) --> [vamos].

%Definición de los sustantivos válidos para las oraciones.
n(n(X),m,sg) --> [DrWaze, Waze, DrWazeLog].
n(n(X),m,sg) --> [X], {lugares(X)}.