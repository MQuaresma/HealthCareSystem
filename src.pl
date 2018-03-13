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

utente(1,"Joao Vaz",23,"São João do Monte").
utente(2,"Maria Fonseca",45,"Braga, Portugal").

prestador(1,"José Paiva",33,"Pediatria","Hospital de Braga").
prestador(2,"Simão Carvalho",56,"Oftalmologia","Hospital de Lisboa").

cuidado("2016-05-23",1,1,"Consulta",20).
cuidado("2017-03-12",2,2,"Consulta",32).

%--------------------------------------------

solucoes(T,Q,S):- Q, registar(tmp(T)), fail.
solucoes(T,Q,S):- construir(S,[]).
 
registar(T):- assert(T).

construir(LF,LI):- retract(tmp(T)), construir(LF,[T|LI]).
construir(S,S).

% utente: #IdUt, Nome, Idade, Morada -> {V,F}

% prestador: #IdPrest, Nome, Especialidade, Instituição -> {V,F}

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo -> {V,F}

% Registar utentes, prestadores e cuidados de saúde

% Remover utentes, prestadores e cuidados de saúde

% Identificar utentes por critérios de seleção

% Identificar as instituições prestadoras de cuidados de saúde

% Identificar cuidados de saúde prestados por instituição/cidade/datas
identificaCuidados(Instituicao,Ins,R):- 
        solucoes( cuidado(d,idU,IdP,des,c), (prestador(IdP,No,Esp,Ins), cuidado(d,idU,IdP,des,c)),R).
%identificaCuidados(Cidade,):-
%identificaCuidados(Data,):-

% Identificar os utentes de um prestador/especialidade/instituição

% Identificar cuidados de saúde realizados por utente/instituição/prestador

% Determinar todas as instituições/prestadores a que um utente já recorreu

% Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas
