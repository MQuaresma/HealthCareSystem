%--------------------------------------------------------------------------------------------
% Declarações iniciais

% If single_var_warnings on, warnings are printed when a sentence containing variables not beginning with ‘_’ occurring once only is compiled or consulted.
:- set_prolog_flag( discontiguous_warnings,off ).

% If discontiguous_warnings on, warnings are printed when clauses are not together in source files, and the relevant predicate has not been declared discontiguous.
:- set_prolog_flag( single_var_warnings,off ).

% Dynamic module fail when it comes to an undefined predicate
:- set_prolog_flag( unknown,fail ).

%--------------------------------------------------------------------------------------------
% Definições iniciais

% op(Precedence, Type, Name)
:- op( 900,xfy,'::' ).

:- dynamic '-'/1.

:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.
:- dynamic instituicao/4.
:- dynamic excecao/1.
:- dynamic nulo/1.
:- dynamic (::)/2.

%-------------------------------------------------------------------------------------------- 
%Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
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

%negaçao de negaçao de algo igual a algo
-(-Q):-Q.
nao(nao(Q)):-Q.

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

% a utente Madalena tem 54 anos de idade e detem o id 22, no entanto é desconhecida a sua morada.
utente(22,madalena,54,xpto1).
excecao(utente(IdU,N,I,M)):- utente(IdU,N,I,xpto1).

% é sabido que existe um prestador com o id 23, ortopedia como sua especialidade e pertencente ao Hospital de Braga, ainda que não se conheça o seu nome.
prestador(23,xpto2,ortopedia,hospital_Braga).
excecao(prestador(IdP,N,E,I)):- prestador(IdP,xpto2,E,I).

% o utente com o id 3 recebeu um cuidado do prestador 3 no dia 23 de Maio de 2017 tendo como custo 20 euros, desconhece-se a descrição do serviço prestado. 
cuidado(2017-05-23,3,3,xpto3,20).
excecao(cuidado(D,IdU,IdP,Desc,C)):- cuidado(D,IdU,IdP,xpto3,C).

% a Clinica Fernandes encontra-se representada na base de conhecimento pelo número 24, não sendo conhecida a cidade em que esta clínica está inserida. 
instituicao(24,clinica_Fernandes,clinica,xpto4).
excecao(instituicao(IdI,N,T,C)):- instituicao(IdI,N,T,xpto4).

%Valor Nulo Impreciso (Desconhecido, mas de um conjunto determinado de hipóteses)

% a Mariana é de Leiria e encontra-se representada na base de conhecimento pelo número 25, sabe-se que a sua idade encontra-se entre os 18 e os 34 anos.
excecao(utente(25,mariana,I,leiria)):- I>=18, I=<34.

% o prestador com o id 26 tem como especialidade Enfermagem e trabalha no Hospital de Coimbra, não se sabe se o seu nome é Rute ou Rita.
excecao(prestador(26,rute,enfermagem,hospital_Coimbra)).
excecao(prestador(26,rita,enfermagem,hospital_Coimbra)).

% a consulta realizada no dia 2 de Agosto de 2017 pelo prestador 4 ao utente 2 teve um custo no intervalo [10,24] euros.
excecao(cuidado(2017-08-02,2,4,consulta,C)):- C>=10, C=<24.

% não se sabe se a Clínica Armandes com o id 27 é uma clínica de Lisboa ou de Setúbal.
excecao(instituicao(27,clinica_Armandes,clinica,lisboa)).
excecao(instituicao(27,clinica_Armandes,clinica,setubal)).

%Valor Nulo Interdito (Desconhecido e não permitido conhecer)

% o utente representado pelo número 28 tem como seu nome Zulmira e tem 45 anos, no entanto, ninguém pode conhecer a sua morada por vontade da mesma.
utente(28,zulmira,45,xpto5).
excecao(utente(IdU,N,I,M)):- utente(IdU,N,I,xpto5).
nulo(xpto5).
+utente(IdU,No,I,M):: (solucoes(M,(utente(28,zulmira,45,M),nao(nulo(M))),L),
                       len(L,N),
                       N==0).

% o Centro Carandá tem ao seu dispor um prestador com especialidade em Medicina Geral e cujo seu número no sistema é o 26, este Centro não revela o nome do mesmo de forma a proteger os seus melhores prestadores.
prestador(29,xpto6,medicina_Geral,centro_Caranda).
excecao(prestador(IdP,N,E,I)):- prestador(IdP,xpto6,E,I).
nulo(xpto6).
+prestador(IdP,No,E,I):: (solucoes(No,(prestador(29,No,medicina_Geral,centro_Caranda),nao(nulo(No))),L),
                          len(L,N),
                          N==0).

% calcula-se que o maior custo alguma vez praticado por um cuidado tenha sido realizado no dia 17 de Outubro de 2017 pelo prestador 4 ao utente 5 com a descrição "consulta"; face à polémica instalada o custo da mesma encontra-se completamente em segredo.
cuidado(2017-10-17,5,4,consulta,xpto7).
excecao(cuidado(D,IdU,IdP,Desc,C)):- cuidado(D,IdU,IdP,Desc,xpto7).
nulo(xpto7).
+cuidado(D,IdU,IdP,Desc,C):: (solucoes(C,(cuidado(2017-10-17,5,4,consulta,C),nao(nulo(C))),L),
                              len(L,N),
                              N==0).

% a Clínica Antunes abriu à pouco tempo e é representada na base de conhecimento pelo número 30, no entanto, a administração desta clínica prefere que pelo menos por enquanto a cidade onde esta se encontra seja interdita e impossível de conhecer.
instituicao(30,clinica_Antunes,clinica,xpto8).
excecao(instituicao(IdI,N,T,C)):- instituicao(IdI,N,T,xpto8).
nulo(xpto8).
+instituicao(IdI,No,T,C):: (solucoes(C,(instituicao(30,clinica_Antunes,clinica,C),nao(nulo(C))),L),
                            len(L,N),
                            N==0).

%--------------------------------------------------------------------------------------------
%Invariantes estruturais

%Impede a evolução do utente(positivo) caso já exista conhecimento positivo, negativo ou desconhecido com o mesmo Id
+utente(Id,Nome,Idade,Morada)::(solucoes((X,Y,Z),utente(Id,X,Y,Z),S1),
                                solucoes((X,Y,Z),-utente(Id,X,Y,Z),S2),
                                solucoes((X,Y,Z),excecao(utente(Id,X,Y,Z)),S3),
                                len(S1,N1),
                                len(S2,N2),
                                len(S3,N3),
                                N is N1 + N2 + N3,
                                N=<1).

%Impede a evolução do prestador(positivo) caso já exista conhecimento positivo, negativo ou desconhecido com o mesmo Id
+prestador(Id,Nome,Esp,Inst)::(solucoes((X,Y,Z),prestador(Id,X,Y,Z),S1),
                               solucoes((X,Y,Z),-prestador(Id,X,Y,Z),S2),
                               solucoes((X,Y,Z),excecao(prestador(Id,X,Y,Z)),S3),
                               len(S1,N1),
                               len(S2,N2),
                               len(S3,N3),
                               N is N1 + N2 + N3,
                               N=<1).

%Prestador pertence a instituição válida
+prestador(Id,Nome,Esp,Inst)::(solucoes((IdI,TipoI,CidadeI),instituicao(IdI,Inst,TipoI,CidadeI),S),
                                len(S,N),
                                N>=1).

%Impede a evolução do cuidado(positivo) caso já exista conhecimento positivo, ou negativo ou desconhecido
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

%Impede a evolução da instituição(positivo) caso já exista conhecimento positivo, negativo ou desconhecido com o mesmo Id
+instituicao(Id,Nome,Tipo,Cidade)::(solucoes((Nome,Tipo,Cidade),instituicao(Id,Nome,Tipo,Cidade),S1),
                                    solucoes((Nome,Tipo,Cidade),-instituicao(Id,Nome,Tipo,Cidade),S2),
                                    solucoes((Nome,Tipo,Cidade),excecao(instituicao(Id,Nome,Tipo,Cidade)),S3),
                                    len(S1,N1),
                                    len(S2,N2),
                                    len(S3,N3),
                                    N is N1 + N2 + N3,
                                    N=<1).

%Não existem cuidados referentes a utente
-utente(Id,Nome,Idade,Morada)::(solucoes((Data,IdP,Desc,Custo),cuidado(Data,Id,IdP,Desc,Custo),S),
                                len(S,N),
                                N==0).

%Não existem cuidados referentes a prestador
-prestador(Id,Nome,Esp,Inst)::(solucoes((Data,IdU,Desc,Custo),cuidado(Data,IdU,Id,Desc,Custo),S),
                                len(S,N),
                                N==0).

%Não existem prestadores registados nesta instituicao
-instituicao(Id,Nome,Tipo,Cidade)::(solucoes((IdP,NomeP,Esp),prestador(IdP,NomeP,Esp,Nome),S),
                                    len(S,N),
                                    N==0).

%Impede a evolução do utente(negativo) caso já exista conhecimento positivo, negativo ou desconhecido com o mesmo Id
+(-utente(Id,Nome,Idade,Morada))::(solucoes((X,Y,Z),utente(Id,X,Y,Z),S1),
                                solucoes((X,Y,Z),-utente(Id,X,Y,Z),S2),
                                solucoes((X,Y,Z),excecao(utente(Id,X,Y,Z)),S3),
                                len(S1,N1),
                                len(S2,N2),
                                len(S3,N3),
                                N is N1 + N2 + N3,
                                N=<1).

%Impede a evolução do prestador(negativo) caso já exista conhecimento positivo, negativo ou desconhecido com o mesmo Id
+(-prestador(Id,Nome,Esp,Inst))::(solucoes((X,Y,Z),prestador(Id,X,Y,Z),S1),
                               solucoes((X,Y,Z),-prestador(Id,X,Y,Z),S2),
                               solucoes((X,Y,Z),excecao(prestador(Id,X,Y,Z)),S3),
                               len(S1,N1),
                               len(S2,N2),
                               len(S3,N3),
                               N is N1 + N2 + N3,
                               N=<1).

%Impede a evolução do cuidado(negativo) caso já exista conhecimento positivo, ou negativo ou desconhecido
+(-cuidado(Data,IdU,IdP,Desc,Custo))::(solucoes((Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),S1),
                                    solucoes((Data,IdU,IdP,Desc,Custo),-cuidado(Data,IdU,IdP,Desc,Custo),S2),
                                    solucoes((Data,IdU,IdP,Desc,Custo),excecao(cuidado(Data,IdU,IdP,Desc,Custo)),S3),
                                    len(S1,N1),
                                    len(S2,N2),
                                    len(S3,N3),
                                    N is N1 + N2 + N3,
                                    N=<1).

%Impede a evolução do instituicao(negativo) caso já exista conhecimento positivo, negativo ou desconhecido com o mesmo Id
+(-instituicao(Id,Nome,Tipo,Cidade))::(solucoes((Nome,Tipo,Cidade),instituicao(Id,Nome,Tipo,Cidade),S1),
                                    solucoes((Nome,Tipo,Cidade),-instituicao(Id,Nome,Tipo,Cidade),S2),
                                    solucoes((Nome,Tipo,Cidade),excecao(instituicao(Id,Nome,Tipo,Cidade),S3)),
                                    len(S1,N1),
                                    len(S2,N2),
                                    len(S3,N3),
                                    N is N1 + N2 + N3,
                                    N=<1).

%Impede a evolução de exceção utente(desconhecido) caso já exista conhecimento positivo, negativo com o mesmo Id
+excecao(utente(Id,Nome,Idade,Morada))::(solucoes((X,Y,Z),utente(Id,X,Y,Z),S1),
                                         solucoes((X,Y,Z),-utente(Id,X,Y,Z),S2),
                                         len(S1,N1),
                                         len(S2,N2),
                                         N is N1 + N2,
                                         N=<1).

%Impede a evolução de exceção prestador(desconhecido) caso já exista conhecimento positivo, ou negativo com o mesmo Id
+excecao(prestador(Id,Nome,Esp,Inst))::(solucoes((X,Y,Z),prestador(Id,X,Y,Z),S1),
                                        solucoes((X,Y,Z),-prestador(Id,X,Y,Z),S2),
                                        len(S1,N1),
                                        len(S2,N2),
                                        N is N1 + N2,
                                        N=<1).

%Impede a evolução de exceção cuidado(desconhecido) caso já exista conhecimento positivo ou negativo
+excecao(cuidado(Data,IdU,IdP,Desc,Custo))::(solucoes((Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),S1),
                                             solucoes((Data,IdU,IdP,Desc,Custo),-cuidado(Data,IdU,IdP,Desc,Custo),S2),
                                             len(S1,N1),
                                             len(S2,N2),
                                             N is N1 + N2,
                                             N=<1).

%Impede a evolução de exceção instituição(desconhecido) caso já exista conhecimento positivo ou negativo com o mesmo Id
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

involucao(Termo):-
    solucoes(Inv,-Termo::Inv,S),
    test(S),
    remover(Termo).

%--------------------------------------------------------------------------------------------
%Predicados que permitem a aprendizagem de novo conhecimento

%verifica se todos valores passados como parametro são não nulos: X,Y,Z -> {V,F}
naoNulo(X,Y,Z):-
    nao(nulo(X)),
    nao(nulo(Y)),
    nao(nulo(Z)).

%verifica se de uma dada lista os valores dos factos sao todos nao nulos: Lista -> {V,F} 
naoNuloL([]).

naoNuloL([utente(IdU,N,I,M)|T]):-naoNulo(N,I,M), naoNuloL(T).
naoNuloL([-utente(IdU,N,I,M)|T]):-naoNulo(N,I,M), naoNuloL(T).
naoNuloL([excecao(utente(IdU,N,I,M))|T]):-naoNulo(N,I,M), naoNuloL(T).

naoNuloL([prestador(IdP,N,E,I)|T]):-naoNulo(N,E,I), naoNuloL(T).
naoNuloL([-prestador(IdP,N,E,I)|T]):-naoNulo(N,E,I), naoNuloL(T).
naoNuloL([excecao(prestador(IdP,N,E,I))|T]):-naoNulo(N,E,I), naoNuloL(T).

naoNuloL([cuidado(D,IdU,IdP,Desc,C)|T]):-naoNulo(D,IdU,IdP), nao(nulo(Desc)),nao(nulo(C)), naoNuloL(T).
naoNuloL([-cuidado(D,IdU,IdP,Desc,C)|T]):-naoNulo(D,IdU,IdP), nao(nulo(Desc)),nao(nulo(C)), naoNuloL(T).
naoNuloL([excecao(cuidado(D,IdU,IdP,Desc,C))|T]):-naoNulo(D,IdU,IdP), nao(nulo(Desc)),nao(nulo(C)), naoNuloL(T).

naoNuloL([instituicao(IdI,N,T,C)|T]):-naoNulo(N,T,C), naoNuloL(T).
naoNuloL([-instituicao(IdI,N,T,C)|T]):-naoNulo(N,T,C), naoNuloL(T).
naoNuloL([excecao(instituicao(IdI,N,T,C))|T]):-naoNulo(N,T,C), naoNuloL(T).

%Utente

evolucaoLearn(utente(IdU,Nome,Idade,Morada)):-
                        solucoes(utente(IdU,N,I,M),utente(IdU,N,I,M),L1),
                        naoNuloL(L1),
                        solucoes(-utente(IdU,N2,I2,M2),-utente(IdU,N2,I2,M2),L2),
                        solucoes(excecao(utente(IdU,N3,I3,M3)),excecao(utente(IdU,N3,I3,M3)),L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(utente(IdU,Nome,Idade,Morada)).

evolucaoLearn(-utente(IdU,Nome,Idade,Morada)):-
                        solucoes(utente(IdU,N,I,M),utente(IdU,N,I,M),L1),
                        naoNuloL(L1),
                        solucoes(-utente(IdU,N2,I2,M2),-utente(IdU,N2,I2,M2),L2),
                        solucoes(excecao(utente(IdU,N3,I3,M3)),excecao(utente(IdU,N3,I3,M3)),L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(-utente(IdU,Nome,Idade,Morada)).

evolucaoLearn(excecao(utente(IdU,Nome,Idade,Morada))):-
                        solucoes(utente(IdU,N,I,M),utente(IdU,N,I,M),L1),
                        naoNuloL(L1),
                        solucoes(-utente(IdU,N2,I2,M2),-utente(IdU,N2,I2,M2),L2),
                        removeL(L1),
                        removeL(L2),
                        inserir(excecao(utente(IdU,Nome,Idade,Morada))).

%Prestador

evolucaoLearn(prestador(IdP,Nome,Especialidade,Inst)):-
                        solucoes(prestador(IdP,N,E,I),prestador(IdP,N,E,I),L1),
                        naoNuloL(L1),
                        solucoes(-prestador(IdP,N2,E2,I2),-prestador(IdP,N2,E2,I2),L2),
                        solucoes(excecao(prestador(IdP,N3,E3,I3)),excecao(prestador(IdP,N3,E3,I3)),L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(prestador(IdP,Nome,Especialidade,Inst)).

evolucaoLearn(-prestador(IdP,Nome,Especialidade,Inst)):-
                        solucoes(prestador(IdP,N,E,I),prestador(IdP,N,E,I),L1),
                        naoNuloL(L1),
                        solucoes(-prestador(IdP,N2,E2,I2),-prestador(IdP,N2,E2,I2),L2),
                        solucoes(excecao(prestador(IdP,N3,E3,I3)),excecao(prestador(IdP,N3,E3,I3)),L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(-prestador(IdP,Nome,Especialidade,Inst)).

evolucaoLearn(excecao(prestador(IdP,Nome,Especialidade,Inst))):-
                        solucoes(prestador(IdP,N,E,I),prestador(IdP,N,E,I),L1),
                        naoNuloL(L1),
                        solucoes(-prestador(IdP,N2,E2,I2),-prestador(IdP,N2,E2,I2),L2),
                        removeL(L1),
                        removeL(L2),
                        inserir(excecao(prestador(IdP,Nome,Especialidade,Inst))).

%Cuidado

evolucaoLearn(cuidado(Data,IdU,IdP,Desc,Custo)):-
                        solucoes(cuidado(Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),L1),
                        naoNuloL(L1),
                        solucoes(-cuidado(Data,IdU,IdP,Desc,Custo),-cuidado(Data,IdU,IdP,Desc,Custo),L2),
                        solucoes(excecao(cuidado(Data,IdU,IdP,Desc,Custo)),excecao(cuidado(Data,IdU,IdP,Desc,Custo)),L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(cuidado(Data,IdU,IdP,Desc,Custo)).

evolucaoLearn(-cuidado(Data,IdU,IdP,Desc,Custo)):-
                        solucoes(cuidado(Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),L1),
                        naoNuloL(L1),
                        solucoes(-cuidado(Data,IdU,IdP,Desc,Custo),-cuidado(Data,IdU,IdP,Desc,Custo),L2),
                        solucoes(excecao(cuidado(Data,IdU,IdP,Desc,Custo)),excecao(cuidado(Data,IdU,IdP,Desc,Custo)),L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(-cuidado(Data,IdU,IdP,Desc,Custo)).

evolucaoLearn(excecao(cuidado(Data,IdU,IdP,Desc,Custo))):-
                        solucoes(cuidado(Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),L1),
                        naoNuloL(L1),
                        solucoes(-cuidado(Data,IdU,IdP,Desc,Custo),-cuidado(Data,IdU,IdP,Desc,Custo),L2),
                        removeL(L1),
                        removeL(L2),
                        inserir(excecao(cuidado(Data,IdU,IdP,Desc,Custo))).

%Instituição

evolucaoLearn(instituicao(IdI,Nome,Tipo,Cidade)):-
                        solucoes(instituicao(IdI,N,T,C),instituicao(IdI,N,T,C),L1),
                        naoNuloL(L1),
                        solucoes(-instituicao(IdI,N2,T2,C2),-instituicao(IdI,N2,T2,C2),L2),
                        solucoes(excecao(instituicao(IdI,N3,T3,C3)),excecao(instituicao(IdI,N3,T3,C3)),L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(instituicao(IdI,Nome,Tipo,Cidade)).

evolucaoLearn(-instituicao(IdI,Nome,Tipo,Cidade)):-
                        solucoes(instituicao(IdI,N,T,C),instituicao(IdI,N,T,C),L1),
                        naoNuloL(L1),
                        solucoes(-instituicao(IdI,N2,T2,C2),-instituicao(IdI,N2,T2,C2),L2),
                        solucoes(excecao(instituicao(IdI,N3,T3,C3)),excecao(instituicao(IdI,N3,T3,C3)),L3),
                        removeL(L1),
                        removeL(L2),
                        removeL(L3),
                        inserir(-instituicao(IdI,Nome,Tipo,Cidade)).

evolucaoLearn(excecao(instituicao(IdI,Nome,Tipo,Cidade))):-
                        solucoes(instituicao(IdI,N,T,C),instituicao(IdI,N,T,C),L1),
                        naoNuloL(L1),
                        solucoes(-instituicao(IdI,N2,T2,C2),-instituicao(IdI,N2,T2,C2),L2),
                        removeL(L1),
                        removeL(L2),
                        inserir(excecao(instituicao(IdI,Nome,Tipo,Cidade))).

%inserção de valores nulos de modo a permitir construir conhecimento interdito
%evolucaoLearn: nulo(T) -> {V,F}
evolucaoLearn(nulo(T)):-
        nao(nulo(T)),
        inserir(nulo(T)).

%WARNING - permite duplicação, usar com muito cuidado, permite a inserçao de regras de exceção 
%evolucaoLearnExc: excecao(P),Q -> {V,F}
evolucaoLearnExc(excecao(P),Q):-
        assert((excecao(P):-Q)).

%WARNING - permite duplicação, usar com muito cuidado, permite a inserçao de invariantes
%evolucaoLearnI: Termo, Invariante -> {V,F}
evolucaoLearnI(T,I):-
        assert(T::I).

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

%remove se existe de uma lista de factos
%removeL: Lista -> {V,F}
removeL([]).
removeL([A|C]) :-A, remover(A), removeL(C).
removeL([A|C]) :- removeL(C).

%Regra de teste dos invariantes correspondentes
%test: Lista -> {V,F}
test([]).
test([H|T]):-H,test(T).
