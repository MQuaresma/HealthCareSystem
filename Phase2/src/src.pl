%--------------------------------------------------------------------------------------------
% Declarações iniciais

% If single_var_warnings on, warnings are printed when a sentence containing variables not beginning with ‘_’ occurring once only is compiled or consulted.
:- set_prolog_flag( discontiguous_warnings,off ).

% If discontiguous_warnings on, warnings are printed when clauses are not together in source files, and the relevant predicate has not been declared discontiguous.
:- set_prolog_flag( single_var_warnings,off ).

% Dynamic module fail when it comes to an undefined predicate
:- set_prolog_flag( unknown,fail ).

%--------------------------------------------------------------------------------------------
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------------------------------------------------------------------
demoComp((Q1,Q2),R):-
    demo(Q1,R1),
    demoComp(Q2,R2),
    conjuncao(R1,R2,R).
demoComp((Q1;Q2),R):-
    demo(Q1,R1),
    demoComp(Q2,R2),
    disjuncao(R1,R2,R).
demoComp(QS,R):- demo(QS,R).

conjuncao(R1,R2,verdadeiro):- R1==verdadeiro, R2==verdadeiro.
conjuncao(R1,R2,falso):- R1==falso.
conjuncao(R1,R2,falso):- R2==falso.
conjuncao(R1,R2,desconhecido):- R1==desconhecido, R2==verdadeiro.
conjuncao(R1,R2,desconhecido):- R1==verdadeiro, R2==desconhecido.
conjuncao(R1,R2,desconhecido):- R1==desconhecido, R2==desconhecido.

disjuncao(R1,R2,falso):- R1==falso, R2==falso.
disjuncao(R1,R2,verdadeiro):- R1==verdadeiro.
disjuncao(R1,R2,verdadeiro):- R2==verdadeiro.
disjuncao(R1,R2,desconhecido):- R1==desconhecido, R2==falso.
disjuncao(R1,R2,desconhecido):- R1==falso, R2==desconhecido.
disjuncao(R1,R2,desconhecido):- R1==desconhecido, R2==desconhecido.

%--------------------------------------------------------------------------------------------
% Extensao do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%ver se é preciso: TODO
%-(-Q):-Q.
%nao(nao(Q)):-Q.

%--------------------------------------------------------------------------------------------
% Definições iniciais

% op(Precedence, Type, Name)
:- op( 900,xfy,'::' ).

:- dynamic '-'/1.

:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.
:- dynamic instituicao/4.
excecao
%--------------------------------------------------------------------------------------------
%defenição de regras negativas
% utente: #IdUt, Nome, Idade, Morada -> {V,F}
-utente(IdUt, N, I, M):- nao(utente(IdUt,N,I,M)),
                         nao(excecao(utente(IdUt,N,I,M))).

% prestador: #IdPrest, Nome, Especialidade, Instituição -> {V,F}
-prestador(IdPrest, N, E, I):- nao(prestador(IdPrest,N,E,I)),
                               nao(excecao(prestador(IdPrest,N,E,I))).

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo -> {V,F}
-cuidado(D, IdUt, IdPrest, Desc, C):- nao(cuidado(D,IdUt,IdPrest,Desc,C)),
                                      nao(excecao(cuidado(D,IdUt,IdPrest,Desc,C))).

%Adicional
% instituicao: #IdIt, Nome, Tipo, Cidade -> {V,F}
-instituicao(IdIt, N, T, C):- nao(instituicao(IdIt,N,T,C)),
                         nao(excecao(instituicao(IdIt,N,T,C))).
%--------------------------------------------------------------------------------------------
%Base de Conhecimento

%Conhecimento Positivo
% utente: #IdUt, Nome, Idade, Morada -> {V,F}
utente(1,joao,23,porto).
utente(2,maria,45,braga).
utente(3,joao,45,setubal).
utente(4,joana,25,braga).
utente(5,miguel,33,evora).
utente(6,carla,60,coimbra).
utente(7,marta,56,braga).
utente(8,guilherme,23,santarem).
utente(9,tatiana,76,faro).
utente(10,beatriz,28,chaves).

% prestador: #IdPrest, Nome, Especialidade, Instituição -> {V,F}
prestador(1,jose,pediatria,hospital_Braga).
prestador(2,simao,oftalmologia,hospital_Lisboa).
prestador(3,alfredo,medicina_Geral,hospital_Guimaraes).
prestador(4,margarida,enfermagem,hospital_Coimbra).
prestador(5,antonio,medicina_Geral,hospital_Coimbra).
prestador(6,mafalda,ortopedia,clinica_SantaTecla).

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo -> {V,F}
cuidado(2016-05-23,1,1,consulta,20).
cuidado(2017-03-04,2,2,consulta,32).
cuidado(2017-05-20,1,3,consulta,15).
cuidado(2017-05-10,1,3,consulta,15).
cuidado(2017-02-14,3,5,consulta,14).
cuidado(2016-11-01,7,4,consulta,10).
cuidado(2017-09-27,8,2,consulta,24).
cuidado(2016-08-30,6,5,consulta,16).
cuidado(2017-01-20,1,6,consulta,10).

%Adicional
% instituicao: #IdIt, Nome, Tipo, Cidade -> {V,F}
instituicao(1,hospital_Braga,hospital,braga).
instituicao(2,hospital_Lisboa,hospital,lisboa).
instituicao(3,hospital_Guimaraes,hospital,guimaraes).
instituicao(4,hospital_Coimbra,hospital,coimbra).
instituicao(5,centro_Caranda,centro_de_saude,braga).
instituicao(6,clinica_SantaTecla,clinica,braga).

%Conhecimento Negativo
% utente: #IdUt, Nome, Idade, Morada -> {V,F}
-utente(101,toze,34,barcelona).
-utente(102,santiago,45,vigo).
-utente(103,sanches,63,huelva).

% prestador: #IdPrest, Nome, Especialidade, Instituição -> {V,F}
-prestador(45,teofilo,mecanico,autoFernandes).
-prestador(46,zacarias,motorista,uber).
-prestador(47,josefina,ortopedia,hospital_Santarem).

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo -> {V,F}
-cuidado(2017-03-04,55,56,consulta,100).
-cuidado(2017-12-23,57,58,consulta,130).
-cuidado(2017-02-13,59,60,consulta,176).

%Adicional
% instituicao: #IdIt, Nome, Tipo, Cidade -> {V,F}
-instituicao(73,marias,hospital,coimbra).
-instituicao(74,timtim,hospital,faro).
-instituicao(75,sofia,hospital,funchal).

%conhecimento imperfeito
%Valor Nulo Incerto (Desconhecido)
utente(22,madalena,54,xpto1).
excecao(utente(IdU,N,I,M)):- utente(IdU,N,I,xpto1).

prestador(23,xpto2,ortopedia,hospital_Braga).
excecao(prestador(IdP,N,E,I)):- prestador(IdP,xpto2,E,I).

cuidado(2017-05-23,3,3,xpto3,20).
excecao(cuidado(D,IdU,IdP,Desc,C)):- cuidado(D,IdU,IdP,xpto3,C).

instituicao(24,clinica_Fernandes,clinica,xpto4).
excecao(instituicao(IdI,N,T,C)):- instituicao(IdI,N,T,xpto4).

%Valor Nulo Impreciso (Desconhecido, mas de um conjunto determinado de hipóteses)
excecao(utente(25,mariana,I,leiria)):- I>=18, I=<34.

excecao(prestador(26,rute,enfermagem,hospital_Coimbra)).
excecao(prestador(26,rita,enfermagem,hospital_Coimbra)).

excecao(cuidado(2017-08-02,2,4,consulta,C)):- C>=10, C=<24.

excecao(instituicao(27,clinica_Armandes,clinica,lisboa)).
excecao(instituicao(27,clinica_Armandes,clinica,setubal)).

%Valor Nulo Interdito (Desconhecido e não permitido conhecer)
utente(28,zulmira,45,xpto5).
excecao(utente(IdU,N,I,M)):- utente(IdU,N,I,xpto5).
nulo(xpto5).
+utente(IdU,No,I,M):: (solucoes(M,(utente(28,zulmira,45,M),nao(nulo(M))),L),
                       len(L,N),
                       N==0).

prestador(29,xpto6,medicina_Geral,centro_Caranda).
excecao(prestador(IdP,N,E,I)):- prestador(IdP,xpto6,E,I).
nulo(xpto6).
+prestador(IdP,No,E,I):: (solucoes(No,(prestador(29,No,medicina_Geral,centro_Caranda),nao(nulo(No))),L),
                          len(L,N),
                          N==0).

cuidado(2017-10-17,5,4,consulta,xpto7).
excecao(cuidado(D,IdU,IdP,Desc,C)):- cuidado(D,IdU,IdP,Desc,xpto7).
nulo(xpto7).
+cuidado(D,IdU,IdP,Desc,C):: (solucoes(C,(cuidado(2017-10-17,5,4,consulta,C),nao(nulo(C))),L),
                              len(L,N),
                              N==0).

instituicao(30,clinica_Antunes,clinica,xpto8).
excecao(instituicao(IdI,N,T,C)):- instituicao(IdI,N,T,xpto8).
nulo(xpto8).
+instituicao(IdI,No,T,C):: (solucoes(C,(instituicao(30,clinica_Antunes,clinica,C),nao(nulo(C))),L),
                            len(L,N),
                            N==0).

%--------------------------------------------------------------------------------------------
%Invariantes estruturais

%Utente com Id nao existe/repetido
+utente(Id,Nome,Idade,Morada)::(solucoes((X,Y,Z),utente(Id,X,Y,Z),S1),
                                solucoes((X,Y,Z),-utente(Id,X,Y,Z),S2),
                                solucoes((X,Y,Z),excecao(utente(Id,X,Y,Z)),S3),
                                len(S1,N1),
                                len(S2,N2),
                                len(S3,N3),
                                N is N1 + N2 + N3,
                                N=<1).

%Prestador com Id nao existe/repetido
+prestador(Id,Nome,Esp,Inst)::(solucoes((X,Y,Z),prestador(Id,X,Y,Z),S1),
                               solucoes((X,Y,Z),-prestador(Id,X,Y,Z),S2),
                               solucoes((X,Y,Z),excecao(prestador(Id,X,Y,Z)),S3),
                               len(S1,N1),
                               len(S2,N2),
                               len(S3,N3),
                               N is N1 + N2 + N3,
                               N=<1).

%Prestador pertence a instituicao valida
+prestador(Id,Nome,Esp,Inst)::(solucoes((IdI,TipoI,CidadeI),instituicao(IdI,Inst,TipoI,CidadeI),S),
                                len(S,N),
                                N>=1).

%Cuidado nao existe/repetido
+cuidado(Data,IdU,IdP,Desc,Custo)::(solucoes((Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),S1),
                                    solucoes((Data,IdU,IdP,Desc,Custo),-cuidado(Data,IdU,IdP,Desc,Custo),S2),
                                    solucoes((Data,IdU,IdP,Desc,Custo),excecao(cuidado(Data,IdU,IdP,Desc,Custo)),S3),
                                    len(S1,N1),
                                    len(S2,N2),
                                    len(S3,N3),
                                    N is N1 + N2 + N3,
                                    N=<1).

%Utente existe
+cuidado(Data,IdU,IdP,Desc,Custo)::(solucoes((X,Y,Z),utente(IdU,X,Y,Z),S),
                                    len(S,N),
                                    N>=1).

%Prestador existe
+cuidado(Data,IdU,IdP,Desc,Custo)::(solucoes((X,Y,Z),prestador(IdP,X,Y,Z),S),
                                    len(S,N),
                                    N>=1).

%Instituicao com Id nao existe/repetido
+instituicao(Id,Nome,Tipo,Cidade)::(solucoes((Nome,Tipo,Cidade),instituicao(Id,Nome,Tipo,Cidade),S1),
                                    solucoes((Nome,Tipo,Cidade),-instituicao(Id,Nome,Tipo,Cidade),S2),
                                    solucoes((Nome,Tipo,Cidade),excecao(instituicao(Id,Nome,Tipo,Cidade)),S3),
                                    len(S1,N1),
                                    len(S2,N2),
                                    len(S3,N3),
                                    N is N1 + N2 + N3,
                                    N=<1).

%Nao existem cuidados referentes a utente
-utente(Id,Nome,Idade,Morada)::(solucoes((Data,IdP,Desc,Custo),cuidado(Data,Id,IdP,Desc,Custo),S),
                                len(S,N),
                                N==0).

%Nao existem cuidados referentes a prestador
-prestador(Id,Nome,Esp,Inst)::(solucoes((Data,IdU,Desc,Custo),cuidado(Data,IdU,Id,Desc,Custo),S),
                                len(S,N),
                                N==0).

%Nao existem prestadores resgistados nesta instituicao
-instituicao(Id,Nome,Tipo,Cidade)::(solucoes((IdP,NomeP,Esp),prestador(IdP,NomeP,Esp,Nome),S),
                                    len(S,N),
                                    N==0).

%Utente com Id nao existe/repetido
+(-utente(Id,Nome,Idade,Morada))::(solucoes((X,Y,Z),utente(Id,X,Y,Z),S1),
                                solucoes((X,Y,Z),-utente(Id,X,Y,Z),S2),
                                solucoes((X,Y,Z),excecao(utente(Id,X,Y,Z)),S3),
                                len(S1,N1),
                                len(S2,N2),
                                len(S3,N3),
                                N is N1 + N2 + N3,
                                N=<1).

%Prestador com Id nao existe/repetido
+(-prestador(Id,Nome,Esp,Inst))::(solucoes((X,Y,Z),prestador(Id,X,Y,Z),S1),
                               solucoes((X,Y,Z),-prestador(Id,X,Y,Z),S2),
                               solucoes((X,Y,Z),excecao(prestador(Id,X,Y,Z)),S3),
                               len(S1,N1),
                               len(S2,N2),
                               len(S3,N3),
                               N is N1 + N2 + N3,
                               N=<1).

%Cuidado nao existe/repetido
+(-cuidado(Data,IdU,IdP,Desc,Custo))::(solucoes((Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),S1),
                                    solucoes((Data,IdU,IdP,Desc,Custo),-cuidado(Data,IdU,IdP,Desc,Custo),S2),
                                    solucoes((Data,IdU,IdP,Desc,Custo),excecao(cuidado(Data,IdU,IdP,Desc,Custo)),S3),
                                    len(S1,N1),
                                    len(S2,N2),
                                    len(S3,N3),
                                    N is N1 + N2 + N3,
                                    N=<1).

%Instituicao com Id nao existe/repetido
+(-instituicao(Id,Nome,Tipo,Cidade))::(solucoes((Nome,Tipo,Cidade),instituicao(Id,Nome,Tipo,Cidade),S1),
                                    solucoes((Nome,Tipo,Cidade),-instituicao(Id,Nome,Tipo,Cidade),S2),
                                    solucoes((Nome,Tipo,Cidade),excecao(instituicao(Id,Nome,Tipo,Cidade),S3)),
                                    len(S1,N1),
                                    len(S2,N2),
                                    len(S3,N3),
                                    N is N1 + N2 + N3,
                                    N=<1).

%Utente com Id nao existe/repetido
+excecao(utente(Id,Nome,Idade,Morada))::(solucoes((X,Y,Z),utente(Id,X,Y,Z),S1),
                                         solucoes((X,Y,Z),-utente(Id,X,Y,Z),S2),
                                         len(S1,N1),
                                         len(S2,N2),
                                         N is N1 + N2,
                                         N=<1).

%Prestador com Id nao existe/repetido
+excecao(prestador(Id,Nome,Esp,Inst))::(solucoes((X,Y,Z),prestador(Id,X,Y,Z),S1),
                                        solucoes((X,Y,Z),-prestador(Id,X,Y,Z),S2),
                                        len(S1,N1),
                                        len(S2,N2),
                                        N is N1 + N2,
                                        N=<1).

%Cuidado nao existe/repetido
+excecao(cuidado(Data,IdU,IdP,Desc,Custo))::(solucoes((Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),S1),
                                             solucoes((Data,IdU,IdP,Desc,Custo),-cuidado(Data,IdU,IdP,Desc,Custo),S2),
                                             len(S1,N1),
                                             len(S2,N2),
                                             N is N1 + N2,
                                             N=<1).

%Instituicao com Id nao existe/repetido
+excecao(instituicao(Id,Nome,Tipo,Cidade))::(solucoes((Nome,Tipo,Cidade),instituicao(Id,Nome,Tipo,Cidade),S1),
                                             solucoes((Nome,Tipo,Cidade),-instituicao(Id,Nome,Tipo,Cidade),S2),
                                             len(S1,N1),
                                             len(S2,N2),
                                             N is N1 + N2,
                                             N=<1).

%--------------------------------------------------------------------------------------------
%Extensao do predicado que permite a evolucao/involucao do conhecimento

evolucao(Termo):-
    solucoes(Inv,+Termo::Inv,S),
    inserir(Termo),
    test(S).

naoNulo(X,Y,Z):-
    nao(nulo(X)),
    nao(nulo(Y)),
    nao(nulo(Z)).

naoNuloL([]).
naoNuloL([utente(IdU,N,I,M)|T]):-naoNulo(N,I,M), naoNuloL(T).
naoNuloL([-utente(IdU,N,I,M)|T]):-naoNulo(N,I,M), naoNuloL(T).
naoNuloL([excecao(utente(IdU,N,I,M))|T]):-naoNulo(N,I,M), naoNuloL(T).

evolucaoLearn(utente(IdU,Nome,Idade,Morada)):-
                        solucoes(utente(IdU,N,I,M),utente(IdU,N,I,M),L1),
                        naoNuloL(L1),
                        solucoes(-utente(IdU,N2,I2,M2),-utente(IdU,N2,I2,M2),L2),
                        naoNuloL(L2),
                        solucoes(excecao(utente(IdU,N3,I3,M3)),excecao(utente(IdU,N3,I3,M3)),L3),
                        naoNuloL(L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(utente(IdU,Nome,Idade,Morada)).

evolucaoLearn(-utente(IdU,Nome,Idade,Morada)):-
                        solucoes(utente(IdU,N,I,M),utente(IdU,N,I,M),L1),
                        naoNuloL(L1),
                        solucoes(-utente(IdU,N2,I2,M2),-utente(IdU,N2,I2,M2),L2),
                        naoNuloL(L2),
                        solucoes(excecao(utente(IdU,N3,I3,M3)),excecao(utente(IdU,N3,I3,M3)),L3),
                        naoNuloL(L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(-utente(IdU,Nome,Idade,Morada)).

evolucaoLearn(excecao(utente(IdU,Nome,Idade,Morada))):-
                        solucoes(utente(IdU,N,I,M),utente(IdU,N,I,M),L1),
                        naoNuloL(L1),
                        solucoes(-utente(IdU,N2,I2,M2),-utente(IdU,N2,I2,M2),L2),
                        naoNuloL(L2),
                        removeL(L1),
                        removeL(L2),
                        inserir(excecao(utente(IdU,Nome,Idade,Morada))).

involucao(Termo):-
    solucoes(Inv,-Termo::Inv,S),
    test(S),
    remover(Termo).

%--------------------------------------------------------------------------------------------
%regras auxiliares

%Encontra todos os predicados(Questao) que sejam satisfeitos ao efetuar o backtracking tendo Formato em conta
%solucoes : Formato, Questao, Soluçoes -> {V,F}
solucoes(T,Q,S):-findall(T,Q,S).

%Calcula o comprimento de uma lista
%len: Lista,Solucao -> {V,F}
len(S,N):-length(S,N).

%Inserir conhecimento
%inserir: Termo -> {V,F}
inserir(P):-assert(P).
inserir(P):-retract(P),!,fail.

%Remover conhecimento
%remover: Termo -> {V,F}
remover(P):-retract(P).

removeL([]).
removeL([A|C]) :- remover(A), removeL(C).

%Regra de teste dos invariantes correspondentes
%test: Lista -> {V,F}
test([]).
test([H|T]):-H,test(T).

%concatenar duas listas
concatenar([],L,L).
concatenar([C|L],L1,[C|R]):-concatenar(L,L1,R).
