%--------------------------------------------
% Declarações iniciais

% If single_var_warnings on, warnings are printed when a sentence containing variables not beginning with ‘_’ occurring once only is compiled or consulted.
:- set_prolog_flag( discontiguous_warnings,off ).

% If discontiguous_warnings on, warnings are printed when clauses are not together in source files, and the relevant predicate has not been declared discontiguous.
:- set_prolog_flag( single_var_warnings,off ).

% Dynamic module fail when it comes to an undefined predicate
:- set_prolog_flag( unknown,fail ).

%--------------------------------------------
% Definições iniciais

% op(Precedence, Type, Name)
:- op( 900,xfy,'::' ).

:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.

%-------------------------------------------
%Base de Conhecimento

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

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo -> {V,F}
cuidado(2016-05-23,1,1,consulta,20).
cuidado(2017-03-04,2,2,consulta,32).
cuidado(2017-05-20,1,3,consulta,15).

%--------------------------------------------
%Funcionalidades

solucoes(T,Q,S):-findall(T,Q,S).

%Registar utentes, prestadores e cuidados de saúde
registarUtente(Id,Nome,Idade,Morada):-evolucao(utente(Id,Nome,Idade,Morada)).

registarPrestador(Id,Nome,Esp,Inst):-evolucao(prestador(Id,Nome,Esp,Inst)).

registarCuidado(Data,IdU,IdPrest,Desc,Custo):-evolucao(cuidado(Data,IdU,IdPrest,Desc,Custo)).

%Remover utentes, prestadores e cuidados de saúde
removerUtente(Id,Nome,Idade,Morada):-involucao(utente(Id,Nome,Idade,Morada)).

removerPrestador(Id,Nome,Esp,Inst):-involucao(prestador(Id,Nome,Esp,Inst)).

removerCuidado(Data,IdU,IdPrest,Desc,Custo):-involucao(cuidado(Data,IdU,IdPrest,Desc,Custo)).

%Identificar utentes por critérios de seleção
%identificaUtente : nome, NomeUtente, Solução -> {V,F}
identificaUtente(nome,Nome,S) :-
		solucoes(utente(X,Nome,Y,Z),utente(X,Nome,Y,Z),S).
%identificaUtente : idade, IdadeUtente, Solução -> {V,F}		
identificaUtente(idade,Idade,S) :-
		solucoes(utente(X,Y,Idade,Z),utente(X,Y,Idade,Z),S).
%identificaUtente : morada, MoradaUtente, Solução -> {V,F}		
identificaUtente(morada,Morada,S) :-
		solucoes(utente(X,Y,Z,Morada),utente(X,Y,Z,Morada),S).

%Identificar as instituições prestadoras de cuidados de saúde
%identificaInstituicoes : Solução -> {V,F}
identificaInstituicoes(S) :-
		solucoes((I),(prestador(X,Y,Z,I)),S).

%Identificar cuidados de saúde prestados por instituição/cidade/datas
%identCuiPrest : instituicao, Instituicao, Resultado -> {V,F}
identCuidPrest(instituicao,Ins,R):- 
        solucoes( Esp, prestador(IdP,No,Esp,Ins),R).
%identCuiPrest : cidade, Cidade, Resultado -> {V,F}
identCuidPrest(cidade,Cid,R):-
        solucoes( Esp, (cuidado(Da,IdU,IdP,Desc,C), utente(IdU,NoU,Id,Cid), prestador(IdP,NoP,Esp,Ins)),R).
%identCuiPrest : datas, Data, Data, Resultado -> {V,F}
identCuidPrest(datas,Data1,Data2,R):-
        solucoes( Esp, (prestador(IdP,No,Esp,Ins), cuidado(Da,IdU,IdP,Desc,C), Da @< Data2, Data1 @< Da),R).

%Identificar os utentes de um prestador/especialidade/instituição
%identUtentes : prestador, Nome, Resultado -> {V,F}
identUtentes(prestador,PrestadorNome,R):-
        solucoes( NoU, (prestador(IdP,PrestadorNome,Esp,Ins), cuidado(Da,IdU,IdP,Desc,C), utente(IdU,NoU,Id,Cid)),R).
%identUtentes : especialidade, Especialidade, Resultado -> {V,F}
identUtentes(especialidade,Espec,R):-
        solucoes( NoU, (prestador(IdP,NoP,Espec,Ins), cuidado(Da,IdU,IdP,Desc,C), utente(IdU,NoU,Id,Cid)),R).
%identUtentes : instituicao, Instituicao, Resultado -> {V,F}
identUtentes(instituicao,Ins,R):-
        solucoes( NoU, (prestador(IdP,NoP,Esp,Ins), cuidado(Da,IdU,IdP,Desc,C), utente(IdU,NoU,Id,Cid)),R).

%Identificar cuidados de saúde realizados por utente/instituição/prestador
identificaCuidadosRealizados(utente,IdU,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), cuidado(Da,IdU,IdP,Desc,C),R).

identificaCuidadosRealizados(instituicao,Ins,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).

identificaCuidadosRealizados(prestador,IdP,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), cuidado(Da,IdU,IdP,Desc,C),R).

%Determinar todas as instituições/prestadores a que um utente já recorreu
porUtente(instituicao,IdU,R):-
		solucoes( Ins, (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).
porUtente(prestador,IdU,R):-
		solucoes( (IdP,No), (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).

%Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas

%funçao auxiliar para somar uma lista
somaLista([],0).
somaLista([X],X).
somaLista([X|L],N):-  
         somaLista(L,N1),
         N is X + N1.

custoTotal(utente,IdU,R):-
		solucoes( C, cuidado(Da,IdU,IdP,Desc,C),Lista),
		somaLista(Lista,R).

custoTotal(especialidade,Esp,R):-
		solucoes( C, (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),Lista),
		somaLista(Lista,R).

custoTotal(prestador,IdP,R):-
		solucoes( C, cuidado(Da,IdU,IdP,Desc,C),Lista), %quando é adicionado um cuidado é verificado se o prestador existe certo? logo pode-se fazer assim, faço o mesmo em cima 
		somaLista(Lista,R).

custoTotal(datas,Data1,Data2 ,R):-
		solucoes( C, (cuidado(Da,IdU,IdP,Desc,C), Da @< Data2, Data1 @< Da),Lista),
		somaLista(Lista,R).

%--------------------------------------------
%Invariantes estruturais
%Utente com Id nao existe/repetido
+utente(Id,Nome,Idade,Morada)::(solucoes((X,Y,Z),utente(Id,X,Y,Z),S),
                                len(S,N),
                                N=<1).
%Prestador com Id nao existe/repetido
+prestador(Id,Nome,Esp,Inst)::(solucoes((X,Y,Z),prestador(Id,X,Y,Z),S),
                                len(S,N),
                                N=<1).
%Cuidado nao existe/repetido
+cuidado(Data,IdU,IdP,Desc,Custo)::(solucoes((Data,IdU,IdP,Desc,Custo),cuidado(Data,IdU,IdP,Desc,Custo),S),
                                len(S,N),
                                N=<1).

%Utente existe
+cuidado(Data,IdU,IdP,Desc,Custo)::(solucoes((X,Y,Z),utente(IdU,X,Y,Z),S),
                                    len(S,N),
                                    N>=1).

%Prestador existe
+cuidado(Data,IdU,IdP,Desc,Custo)::(solucoes((X,Y,Z),prestador(IdP,X,Y,Z),S),
                                    len(S,N),
                                    N>=1).

%Nao existem cuidados referentes a utente
-utente(Id,Nome,Idade,Morada)::(solucoes((Data,IdP,Desc,Custo),cuidado(Data,Id,IdP,Desc,Custo),S),
                                len(S,N),
                                N=<1).

%Nao existem cuidados referentes a prestador
-prestador(Id,Nome,Esp,Inst)::(solucoes((Data,IdU,Desc,Custo),cuidado(Data,IdU,Id,Desc,Custo),S),
                                len(S,N),
                                N=<1).
%--------------------------------------------
%Extensao do predicado que permite a evolucao/involucao do conhecimento

inserir(P):-assert(P).
inserir(P):-retract(P),!,fail.

remover(P):-retract(P).
remover(P):-assert(P),!,fail.

test([]).
test([H|T]):-H,test(T).

len(S,N):-length(S,N).

evolucao(Termo):-
    solucoes(Inv,+Termo::Inv,S),
    inserir(Termo),
    test(S).

involucao(Termo):-
    solucoes(Inv,-Termo::Inv,S),
    remover(Termo),
    test(S).

%--------------------------------------------
%Extras

% Determinar as especialidades com que um utente esteve relacionado, devolvendo a data das mesmas.
% especialidadesDeUtente : IdUtente, Solução -> {V,F}
especialidadesDeUtente(IdU,S) :-
		solucoes((Esp,Data),(utente(IdU,NomeU,IdadeU,Morada),prestador(IdP,X,Esp,Y),cuidado(Data,IdU,IdP,Z,W)),S).

% Determinar os prestadores que cuidaram de um utente.
% prestadoresDeUtenteEmInstituicao : IdUtente, Solução -> {V,F}
prestadoresDeUtenteEmInstituicao(IdU,Inst,S) :-
		solucoes(prestadores(IdP,NomeP,Esp,Inst),(utente(IdU,NomeU,IdadeU,Morada),prestador(IdP,NomeP,Esp,Inst),cuidado(Data,IdU,IdP,Desc,Custo)),S).
