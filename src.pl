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

% prestador: #IdPrest, Nome, Especialidade, Instituição -> {V,F}
prestador(1,jose,pediatria,hospital_Braga).
prestador(2,simao,oftalmologia,hospital_Lisboa).

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo -> {V,F}
cuidado(2016-05-23,1,1,consulta,20).
cuidado(2017-03-04,2,2,consulta,32).

%--------------------------------------------
%Funcionalidades

solucoes(T,Q,S):-findall(T,Q,S).

% Registar utentes, prestadores e cuidados de saúde

% Remover utentes, prestadores e cuidados de saúde

% Identificar utentes por critérios de seleção
identificaUtente(nome,Nome,S) :-
		solucoes(utente(X,Nome,Y,Z),utente(X,Nome,Y,Z),S).
identificaUtente(idade,Idade,S) :-
		solucoes(utente(X,Y,Idade,Z),utente(X,Y,Idade,Z),S).
identificaUtente(morada,Morada,S) :-
		solucoes(utente(X,Y,Z,Morada),utente(X,Y,Z,Morada),S).

% Identificar as instituições prestadoras de cuidados de saúde
identificaInstituicoes(S) :-
		solucoes((I),(prestador(X,Y,Z,I)),S).

% Identificar cuidados de saúde prestados por instituição/cidade/datas
% identCuiPrest : instituicao, Instituicao, Resultado -> {V,F}
identCuidPrest(instituicao,Ins,R):- 
        solucoes( Esp, prestador(IdP,No,Esp,Ins),R).
% identCuiPrest : cidade, Cidade, Resultado -> {V,F}
identCuidPrest(cidade,Cid,R):-
        solucoes( Esp, (cuidado(Da,IdU,IdP,Desc,C), utente(IdU,NoU,Id,Cid), prestador(IdP,NoP,Esp,Ins)),R).
% identCuiPrest : datas, Data, Data, Resultado -> {V,F}
identCuidPrest(datas,Data1,Data2,R):-
        solucoes( Esp, (prestador(IdP,No,Esp,Ins), cuidado(Da,IdU,IdP,Desc,C), Da @< Data2, Data1 @< Da),R).

% Identificar os utentes de um prestador/especialidade/instituição
% identUtentes : prestador, Nome, Resultado -> {V,F}
identUtentes(prestador,PrestadorNome,R):-
        solucoes( NoU, (prestador(IdP,PrestadorNome,Esp,Ins), cuidado(Da,IdU,IdP,Desc,C), utente(IdU,NoU,Id,Cid)),R).
% identUtentes : especialidade, Especialidade, Resultado -> {V,F}
identUtentes(especialidade,Espec,R):-
        solucoes( NoU, (prestador(IdP,NoP,Espec,Ins), cuidado(Da,IdU,IdP,Desc,C), utente(IdU,NoU,Id,Cid)),R).
% identUtentes : instituicao, Instituicao, Resultado -> {V,F}
identUtentes(instituicao,Ins,R):-
        solucoes( NoU, (prestador(IdP,NoP,Esp,Ins), cuidado(Da,IdU,IdP,Desc,C), utente(IdU,NoU,Id,Cid)),R).

% Identificar cuidados de saúde realizados por utente/instituição/prestador
identificaCuidadosRealizados(utente,IdU,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), cuidado(Da,IdU,IdP,Desc,C),R).

identificaCuidadosRealizados(instituicao,Ins,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).

identificaCuidadosRealizados(prestador,IdP,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), cuidado(Da,IdU,IdP,Desc,C),R).

% Determinar todas as instituições/prestadores a que um utente já recorreu
porUtente(instituicao,IdU,R):-
		solucoes( Ins, (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).
porUtente(prestador,IdU,R):-
		solucoes( (IdP,No), (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).
% Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas

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