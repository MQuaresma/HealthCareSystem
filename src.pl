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

utente(1,joao,23,porto).
utente(2,maria,45,braga).

prestador(1,jose,pediatria,hospital_Braga).
prestador(2,simao,oftalmologia,hospital_Lisboa).

cuidado(janeiro,1,1,consulta,20).
cuidado(fevereiro,2,2,consulta,32).

%--------------------------------------------

solucoes(T,Q,S):-findall(T,Q,S).

% utente: #IdUt, Nome, Idade, Morada -> {V,F}

% prestador: #IdPrest, Nome, Especialidade, Instituição -> {V,F}

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo -> {V,F}

% Registar utentes, prestadores e cuidados de saúde

% Remover utentes, prestadores e cuidados de saúde

% Identificar utentes por critérios de seleção

% Identificar as instituições prestadoras de cuidados de saúde
identificaInstituicoes(S) :-
		solucoes((I),(prestador(X,Y,Z,I)),S).

% Identificar cuidados de saúde prestados por instituição/cidade/datas
identificaCuidados(instituicao,Ins,R):- 
        solucoes( cuidado(Da,IdU,IdP,Desc,C), (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).
%identificaCuidados(Cidade,):-
%identificaCuidados(Data,):-

% Identificar os utentes de um prestador/especialidade/instituição

% Identificar cuidados de saúde realizados por utente/instituição/prestador

% Determinar todas as instituições/prestadores a que um utente já recorreu

% Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas
