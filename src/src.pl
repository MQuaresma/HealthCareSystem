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

:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.
:- dynamic instituicao/4.

%--------------------------------------------------------------------------------------------
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
prestador(6,mafalda,ortopedia,clinica_SantaTecla).

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo -> {V,F}
cuidado(2016-05-23,1,1,consulta,20).
cuidado(2017-03-04,2,2,consulta,32).
cuidado(2017-05-20,1,3,consulta,15).
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

%--------------------------------------------------------------------------------------------
%Funcionalidades

%--------------------------------------------------------------------------------------------
%Registar utentes, prestadores e cuidados de saúde,instituicoes

%registarUtente: Id, Nome, Idade, Morada -> {V,F}
registarUtente(Id,Nome,Idade,Morada):-evolucao(utente(Id,Nome,Idade,Morada)).

%registarPrestador: Id, Nome, Especialidade, Instituição -> {V,F}
registarPrestador(Id,Nome,Esp,Inst):-evolucao(prestador(Id,Nome,Esp,Inst)).

%registarCuidado: Data, IdUtente, IdPrestador, Descrição, Custo -> {V,F}
registarCuidado(Data,IdU,IdPrest,Desc,Custo):-evolucao(cuidado(Data,IdU,IdPrest,Desc,Custo)).

%registarInstituicao: Id, Nome, Tipo, Cidade -> {V,F}
registarInstituicao(Id,Nome,Tipo,Cidade):-evolucao(instituicao(Id,Nome,Tipo,Cidade)).

%--------------------------------------------------------------------------------------------
%Remover utentes, prestadores e cuidados de saúde,instituicoes

%removerUtente: Id, Nome, Idade, Morada -> {V,F}
removerUtente(Id,Nome,Idade,Morada):-involucao(utente(Id,Nome,Idade,Morada)).

%removerPrestador: Id, Nome, Especialidade, Instituição -> {V,F}
removerPrestador(Id,Nome,Esp,Inst):-involucao(prestador(Id,Nome,Esp,Inst)).

%removerCuidado: Data, IdUtente, IdPrestador, Descrição, Custo -> {V,F}
removerCuidado(Data,IdU,IdPrest,Desc,Custo):-involucao(cuidado(Data,IdU,IdPrest,Desc,Custo)).

%removerInstituicao: Id, Nome, Tipo, Cidade -> {V,F}
removerInstituicao(Id,Nome,Tipo,Cidade):-involucao(instituicao(Id,Nome,Tipo,Cidade)).

%--------------------------------------------------------------------------------------------
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

%--------------------------------------------------------------------------------------------
%Identificar as instituições prestadoras de cuidados de saúde

%identificaInstituicoes : Solução -> {V,F}
identificaInstituicoes(S) :-
		solucoes((I),(prestador(X,Y,Z,I)),S).

%--------------------------------------------------------------------------------------------
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

%--------------------------------------------------------------------------------------------
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

%--------------------------------------------------------------------------------------------
%Identificar cuidados de saúde realizados por utente/instituição/prestador

%identificaCuidadosRealizados: utente, IdUtente, Resultado -> {V,F}
identificaCuidadosRealizados(utente,IdU,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), cuidado(Da,IdU,IdP,Desc,C),R).

%identificaCuidadosRealizados: instituicao, Instituição, Resultado -> {V,F}
identificaCuidadosRealizados(instituicao,Ins,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).

%identificaCuidadosRealizados: prestador, IdPrestador, Resultado -> {V,F}
identificaCuidadosRealizados(prestador,IdP,R):-
        solucoes( cuidado(Da,IdU,IdP,Desc,C), cuidado(Da,IdU,IdP,Desc,C),R).

%--------------------------------------------------------------------------------------------
%Determinar todas as instituições/prestadores a que um utente já recorreu

%porUtente: instituicao, IdUtente, Resultado -> {V,F} 
porUtente(instituicao,IdU,R):-
		solucoes( Ins, (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).

%porUtente: prestador, IdUtente, Resultado -> {V,F}
porUtente(prestador,IdU,R):-
		solucoes( (IdP,No), (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),R).

%--------------------------------------------------------------------------------------------
%Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas

%custoTotal: utente, IdUtente, Resultado -> {V,F}
custoTotal(utente,IdU,R):-
		solucoes( C, cuidado(Da,IdU,IdP,Desc,C),Lista),
		somaLista(Lista,R).

%custoTotal: especialidade, Especialidade, Resultado -> {V,F}
custoTotal(especialidade,Esp,R):-
		solucoes( C, (cuidado(Da,IdU,IdP,Desc,C), prestador(IdP,No,Esp,Ins)),Lista),
		somaLista(Lista,R).

%custoTotal: prestador, IdPrestador, Resultado -> {V,F}
custoTotal(prestador,IdP,R):-
		solucoes( C, cuidado(Da,IdU,IdP,Desc,C),Lista),
		somaLista(Lista,R).

%custoTotal: datas, Data, Data, Resultado -> {V,F}
custoTotal(datas,Data1,Data2 ,R):-
		solucoes( C, (cuidado(Da,IdU,IdP,Desc,C), Da @< Data2, Data1 @< Da),Lista),
		somaLista(Lista,R).

%--------------------------------------------------------------------------------------------
%Invariantes estruturais

%Utente com Id nao existe/repetido
+utente(Id,Nome,Idade,Morada)::(solucoes((X,Y,Z),utente(Id,X,Y,Z),S),
                                len(S,N),
                                N=<1).

%Prestador com Id nao existe/repetido
+prestador(Id,Nome,Esp,Inst)::(solucoes((X,Y,Z),prestador(Id,X,Y,Z),S),
                                len(S,N),
                                N=<1).

%Prestador pertence a instituicao valida
+prestador(Id,Nome,Esp,Inst)::(solucoes((IdI,TipoI,CidadeI),instituicao(IdI,Inst,TipoI,CidadeI),S),
                                len(S,N),
                                N>=1).

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

%Instituicao com Id nao existe/repetido
+instituicao(Id,Nome,Tipo,Cidade)::(solucoes((Nome,Tipo,Cidade),instituicao(Id,Nome,Tipo,Cidade),S),
                                    len(S,N),
                                    N=<1).

%Nao existem cuidados referentes a utente
-utente(Id,Nome,Idade,Morada)::(solucoes((Data,IdP,Desc,Custo),cuidado(Data,Id,IdP,Desc,Custo),S),
                                len(S,N),
                                N=<1).

%Nao existem cuidados referentes a prestador
-prestador(Id,Nome,Esp,Inst)::(solucoes((Data,IdU,Desc,Custo),cuidado(Data,IdU,Id,Desc,Custo),S),
                                len(S,N),
                                N=<1).

%Nao existem prestadores resgistados nesta instituicao
-instituicao(Id,Nome,Tipo,Cidade):-(solucoes((IdP,NomeP,Esp),prestador(IdP,NomeP,Esp,Nome),S),
                                    len(S,N),
                                    N=<1).

%--------------------------------------------------------------------------------------------
%Extensao do predicado que permite a evolucao/involucao do conhecimento

evolucao(Termo):-
    solucoes(Inv,+Termo::Inv,S),
    inserir(Termo),
    test(S).

involucao(Termo):-
    solucoes(Inv,-Termo::Inv,S),
    remover(Termo),
    test(S).

%--------------------------------------------------------------------------------------------
%regras auxiliares

%solocoes : Formato, Questao, Soluçoes -> {V,F}
solucoes(T,Q,S):-findall(T,Q,S).

%funçao auxiliar para somar uma lista
somaLista([],0).
somaLista([X],X).
somaLista([X|L],N):-  
         somaLista(L,N1),
         N is X + N1.

%comprimento de uma lista
len(S,N):-length(S,N).

%inserir conhecimento
inserir(P):-assert(P).
inserir(P):-retract(P),!,fail.

%remover conhecimento
remover(P):-retract(P).
remover(P):-assert(P),!,fail.

%regra de teste dos invariantes correspondentes
test([]).
test([H|T]):-H,test(T).

%--------------------------------------------------------------------------------------------
%Extras

%Determinar as especialidades com que um utente esteve relacionado, devolvendo a data das mesmas.
%especialidadesDeUtente : IdUtente, Solução -> {V,F}
especialidadesDeUtente(IdU,S) :-
		solucoes((Esp,Data),(utente(IdU,NomeU,IdadeU,Morada),prestador(IdP,X,Esp,Y),cuidado(Data,IdU,IdP,Z,W)),S).

%Determinar os prestadores que cuidaram de um utente.
%prestadoresDeUtenteEmInstituicao : IdUtente, Solução -> {V,F}
prestadoresDeUtenteEmInstituicao(IdU,Inst,S) :-
		solucoes(NomeP,(utente(IdU,NomeU,IdadeU,Morada),prestador(IdP,NomeP,Esp,Inst),cuidado(Data,IdU,IdP,Desc,Custo)),S).

%Determinar as instituições existentes cidade.
%instituicoesDeCidade : Cidade, Solução -> {V,F}
instituicoesDeCidade(Cidade,S) :-
        solucoes(NomeI,instituicao(IdI,NomeI,TipoI,Cidade),S).

%Determinar os tipos de instituições existentes numa cidade.
%tiposDeInstituicoesDeCidade : Cidade, Solução -> {V,F}
tiposDeInstituicoesDeCidade(Cidade,S) :-
        solucoes(TipoI,instituicao(IdI,NomeI,TipoI,Cidade),S).

%Determinar o tipo de instituições que um utente já visitou.
%tiposInstituicoesVisitadasPorUtente : IdUtente, Solução -> {V,F}
tiposInstituicoesVisitadasPorUtente(IdU,S) :-
        solucoes(TipoI,(utente(IdU,NomeU,IdadeU,Morada),prestador(IdP,NomeP,Esp,Inst),cuidado(Data,IdU,IdP,Desc,Custo),instituicao(IdI,Inst,TipoI,Cidade)),S).

%Que instituiçoes sao hospitais/clinicas/centros de saude
%instituicoesDoTipo : Tipo, Solução -> {V,F}
instituicoesDoTipo(Tipo,S):-
        solucoes(NomeI,instituicao(IdI,NomeI,Tipo,Ci),S).

%Em que cidade foram mais cuidados realizados

%Qual a instituiçao com mais cuidados realizados

%Que utente tem mais cuidados realizados

%Que prestador prestou mais cuidados

%Em que instituição o utente realizou mais cuidados
