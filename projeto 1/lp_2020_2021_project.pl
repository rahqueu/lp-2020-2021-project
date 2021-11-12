% Raquel Cardoso - ist199314
:- [codigo_comum].

% ============== AUX ==============

combinacoes(N, Els, Combs) :-
    findall(Comb, (combinacao(N, Els, Comb)), Combs).

permutacoes(N, Els, Perms) :-
    combinacoes(N, Els, Combs),
    findall(V, (member(X, Combs), permutation(X, V)), V2),
    sort(V2, Perms).

tira(Y, [Y], []).
tira(X, [X|Lista1], Lista1).
tira(X, [Y|Lista],[Y|Lista1]) :-
    tira(X, Lista, Lista1), !.

soma_lista([], 0).
soma_lista([P|L], Soma) :-
    soma_lista(L, Soma2),
    Soma is Soma2 + P.

membro(E, [V|_]) :- E==V.
membro(E, [_|R]) :- membro(E, R).

% =============== 1 ===============

/*
combinacoes_soma(N, Els, Soma, Combs).
combinacoes_soma(N, Els, Soma, Combs), em que N e um inteiro, Els e uma
lista de inteiros, e Soma e um inteiro, significa que Combs e a lista ordenada cujos elementos
sao as combinacoes N a N, dos elementos de Els cuja soma e Soma .

Sumario: Encontra as combinacoes para uma dada soma e qauntidade de numeros na mesma
*/

combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, (combinacao(N, Els, Comb), soma_lista(Comb, Soma)), Combs). % lista de todas as combinacoes de Els com 2 elementos e que somem Soma

% =============== 2 ===============

/*
permutacoes_soma(N, Els, Soma, Perms).
permutacoes_soma(N, Els, Soma, Perms), em que N e um inteiro, Els e uma
lista de inteiros, e Soma e um inteiro, significa que Perms e a lista ordenada cujos elementos
sao as permutacoes das combinacoes N a N, dos elementos de Els cuja soma e
Soma.

Sumario: Encontra as permutacoes para uma dada soma e qauntidade de numeros na mesma
*/


permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),                          % chamar combinacoes_soma para encontrar Combs
    findall(V, (member(X, Combs), permutation(X, V)), V2),          % lista de todos os membros de combs e permutacoes desses membros
    sort(V2, Perms). 

% =============== 3 ===============

/*
espaco_fila(Fila, Esp, H_V).
espaco_fila(Fila, Esp, H_V), em que Fila e uma fila (linha ou coluna) de um
puzzle e H_V e um dos atomos h ou v, conforme se trate de uma fila horizontal ou vertical,
respectivamente, significa que Esp e um espaco de Fila, tal como descrito na Seccao 2.1,
no passo 1.

Sumario: cria os espacos para uma fila dada (Horizontal ou Vertical).
*/

criar_espaco(SomaEspaco, Vars, espaco(SomaEspaco, Vars)).

%todos os casos de lista ou variavel
lista_ou_variavel(Fila, LV) :-
    lista_ou_variavel(Fila, LV, []).

lista_ou_variavel([], LV, LV).
lista_ou_variavel([P|_], LV, LVAux) :-
    nonvar(P),                                                      %P nao e uma variavel
    lista_ou_variavel([], LV, LVAux).

lista_ou_variavel([P|R], LV, LVAux) :-
    var(P),                                                         %P e uma variavel
    append(LVAux, [P], LVAux2),                                     %damos append
    lista_ou_variavel(R, LV, LVAux2).

espaco_fila(Fila, Esp, H_V) :-
    bagof(Elemento, (member(Elemento, Fila)), FilaModificada),      %todos os elementos serao fila modificada
    espaco_fila_aux(FilaModificada, ListaEsp, H_V, []),             %chamada da auxiliar para encontrar ListaEsp
    espaco_fila_aux2(ListaEsp, EspacosL1),                          %chamada da aux2 para encontrar EspacosL1
    member(Esp, EspacosL1).                                         %todos os membros de EspacosL1

espaco_fila_aux([], Esp, _, Esp).
espaco_fila_aux([P|R], ListaEsp, H_V, Esp) :-
    H_V == 'h',
    nth1(2, P, Valor),                                  % valor da horizontal
    lista_ou_variavel(R, Vars),                         % encontrar as Vars
    criar_espaco(Valor, Vars, Espaco),                  % ter espaco
    append(Esp, [Espaco], Esp1),                        % adicionar um espaco a lista de espacos
    append([P], Vars, Apagar),                          % fila com P e as Variaveis, vai ser o que queremos apagar
    append([P], R, Fila),                               % a Fila e P e o Resto
    append(Apagar, X, Fila),                            % Apagar + X = Fila logo X = Fila - Apagar
    espaco_fila_aux(X, ListaEsp, H_V, Esp1).            % recursiva

espaco_fila_aux([P|R], ListaEsp, H_V, Esp) :-
    H_V == 'v',
    nth1(1, P, Valor),                                  % valor da vertical
    lista_ou_variavel(R, Vars),                         % encontrar as Vars
    criar_espaco(Valor, Vars, Espaco),                  % ter espaco
    append(Esp, [Espaco], Esp1),                        % adicionar um espaco a lista de espacos
    append([P], Vars, Apagar),                          % fila com P e as Variaveis, vai ser o que queremos apagar
    append([P], R, Fila),                               % a Fila e P e o Resto
    append(Apagar, X, Fila),                            % Apagar + X = Fila logo X = Fila - Apagar
    espaco_fila_aux(X, ListaEsp, H_V, Esp1).            % recursiva

espaco_fila_aux2(EspacosL, EspacosL1) :-
    espaco_fila_aux2(EspacosL, EspacosL1, []).      
espaco_fila_aux2([], Lista, Lista).

espaco_fila_aux2([P|R], EspacosL1, Lista) :-            % recebe EspacosL, EspacosL1 e uma lista vazia a preencher
    criar_espaco(_, Vars, P),                           % retirar as Vars de P
    Vars \== [], !,                                     % so ignorar o espaco caso a lista de variaveis deste seja vazia
    append(Lista, [P], Total),                          % append de P
    espaco_fila_aux2(R, EspacosL1, Total).

espaco_fila_aux2([_|R], EspacosL1, Lista) :-
    espaco_fila_aux2(R, EspacosL1, Lista).

% =============== 4 ===============

/*
espacos_fila(H_V, Fila, Espacos).
espacos_fila(H_V, Fila, Espacos), em que Fila e uma fila (linha ou coluna) de
uma grelha e e H_V e um dos atomos h ou v, significa que Espacos e a lista de todos os
espacos de Fila, da esquerda para a direita.

Sumario:
Lista de todos os espacos de uma Fila
*/

espacos_fila(H_V, Fila, Espacos) :-                     
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).   %todos os Esp serao Espacos
espacos_fila(H_V, Fila, Espacos) :-
    \+bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos), %ou entao espacos e vazio
    Espacos = [].

% =============== 5 ===============

/*
espacos_puzzle(Puzzle, Espacos).
espacos_puzzle(Puzzle, Espacos), em que Puzzle e um puzzle, significa que
Espacos e a lista de espacos de Puzzle, tal como descrito na Seccao 2.1, no passo 1.
Sugestao: use o predicado mat_transposta, definido no ficheiro codigo_comum.pl.

Lista Espacos em que cada indice e um espaco do Puzzle
*/

espacos_puzzle(Puzzle, Espacos) :-
    espacos_puzzle_aux(Puzzle, EspacosNT, []),          %espacos horizontais
    mat_transposta(Puzzle, Transp),                     %mat transposta deixa-nos ver colunas como filas (v como h)
    espacos_puzzle_auxT(Transp, EspacosT, []),          %espacos verticas
    append(EspacosNT, EspacosT, Espacos).               %espacos horizontais + espacos verticais = espacos


espacos_puzzle_aux([], EspacosInicio, EspacosInicio).
espacos_puzzle_aux([P|R], Res, Filas) :-
    espacos_fila('h', P, Esps),                         % encontramos Esps
    append(Filas, Esps, EspacosInicio),                 % podemos adicionar Esps a Filas
    espacos_puzzle_aux(R, Res, EspacosInicio).          % chamada recursiva da funcao
   
espacos_puzzle_auxT([], EspacosT, EspacosT).
espacos_puzzle_auxT([P|R], Res, Filas) :-
    espacos_fila('v', P, EspsT),                        % encontramos EspsT
    append(Filas, EspsT, EspacosT),                     % podemos adicionar EspsT a Filas
    espacos_puzzle_auxT(R, Res, EspacosT).              % chamada recursiva da funcao


% =============== 6 ===============
/*
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com).
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos
e uma lista de espacos e Esp e um espaco, significa que Esps_com e a lista de espacos
com variaveis em comum com Esp, exceptuando Esp. Os espacos em Esps_com devem
aparecer pela mesma ordem que aparecem em Espacos.

Sumario:
Esps_com e a lista de espacos comuns a um espaco.
*/

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    criar_espaco(_, Vars, Esp),
    espacos_com_posicoes_comuns_aux5(Esp, Espacos, ListaEsps),
    espacos_com_posicoes_comuns_aux3(ListaEsps, [], ListaVars), 
    espacos_com_posicoes_comuns_aux4(ListaEsps, [], ListaInt),
    espacos_com_posicoes_comuns_aux2(Vars, ListaVars, ListaInt, Esps_com, []).

% ==== aux que toma uma das vars de Esp e ve se e membro dos espacos
espacos_com_posicoes_comuns_aux(_, [], [], Output, Output).
espacos_com_posicoes_comuns_aux(Var, [Q|U], [P|R], Vazio, Resultado) :-
    membro(Var, Q),
    criar_espaco(P, Q, Espaco),
    append(Vazio, [Espaco], Output),
    espacos_com_posicoes_comuns_aux(Var, U, R, Output, Resultado), !.

espacos_com_posicoes_comuns_aux(Var, [Q|U], [_|R], Vazio, Resultado) :-
    \+ membro(Var, Q),
    espacos_com_posicoes_comuns_aux(Var, U, R, Vazio, Resultado), !.

% ==== aux que chama a outra aux para poder comparar todas as vars de Esp com os membros dos espacos
espacos_com_posicoes_comuns_aux2([], _, _, Res, Res).
espacos_com_posicoes_comuns_aux2([V|X], ListaVars, ListaInt, Resultado, Inicio) :-
    espacos_com_posicoes_comuns_aux(V, ListaVars, ListaInt, [], Lista),
    append(Inicio, Lista, Res),
    espacos_com_posicoes_comuns_aux2(X, ListaVars, ListaInt, Resultado, Res).

%lista so de listas de vars
espacos_com_posicoes_comuns_aux3([], Res, Res).
espacos_com_posicoes_comuns_aux3([P|Q], Fila, Resultado) :-
    criar_espaco(_, Vars, P), %Vars de P
    append(Fila, [Vars], Res),
    espacos_com_posicoes_comuns_aux3(Q, Res, Resultado).

%lista so de somas
espacos_com_posicoes_comuns_aux4([], Res, Res).
espacos_com_posicoes_comuns_aux4([P|Q], Fila, Resultado) :-
    criar_espaco(Soma, _, P), %Soma de P
    append(Fila, [Soma], Res),
    espacos_com_posicoes_comuns_aux4(Q, Res, Resultado).

%tirar Esp
espacos_com_posicoes_comuns_aux5(Esp, Espacos, ListaEsps) :-
    tira(Esp, Espacos, ListaEsps), !.

% =============== 7 ===============

/*
permutacoes_soma_espacos(Espacos, Perms_soma).
permutacoes_soma_espacos(Espacos, Perms_soma), em que Espacos e uma
lista de espacos, significa que Perms_soma e a lista de listas de 2 elementos, em que
o 1o elemento e um espaco de Espacos e o 2o e a lista ordenada de permutacoes cuja
soma e igual o soma do espaco.

Sumario:
Encontramos as permutacoes para todos os espacos dentro de Espacos e criamos uma lista
"Perms_soma" em que cada indice tem o espaco correspondente como primeiro elemento
e as permutacoes possiveis para o espaco como o segundo. */

permutacoes_soma_espacos(Espacos, Perms_soma) :-
    permutacoes_soma_espacos_aux([], Espacos, Perms_soma), !.

permutacoes_soma_espacos_aux(Total, [], Total).
permutacoes_soma_espacos_aux(P, [Q|U], NovaLista) :-
    %encontramos os valores a meter nos elementos de cada indice de P e damos append numa lista que sera Perms_soma
    criar_espaco(Soma, Vars, Q),
    length(Vars, N),
    permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
    append(P, [[Q, Perms]], Total),
    permutacoes_soma_espacos_aux(Total, U, NovaLista), !.

% =============== 8 ===============

/*
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma).
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), em que
Perm e uma permutacao, Esp e um espaco, Espacos e uma lista de espacos, e
Perms_soma e uma lista de listas tal como obtida pelo predicado anterior, significa que
Perm e uma permutacao possivel para o espaco Esp, tal como descrito na Seccao 2.1, no
passo 2.

Sumario:
Encontrar as permutacoes possiveis para um espaco tendo em conta os seus espacos comuns
para saber o que que pode ou nao pode estar
*/

criar_perms(Espaco, Permutacoes, permutacoes(Espaco, Permutacoes)).

%numero espacos em comum = variaveis de esp
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    permutacao_possivel_aux6(Perms_soma, Esp, Perms_soma_Esp),
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), 
    permutacoes_soma_espacos(Esps_com, Perms_soma_com),
    permutacao_possivel_aux4(Perms_soma_com, [], RetirarASoma),
    permutacao_possivel_aux5(RetirarASoma, [], PermsEspCom),
    nth1(1, Perms_soma_Esp, NVariaveis), 
    length(NVariaveis, V),
    length(Esps_com, X),
    X = V,                                                              % numero de variaveis == numero de espacos comuns
    permutacao_possivel_espaco_aux(Perms_soma_Esp, PermsEspCom, [], Lista),
    length(Perms_soma_com, N),
    permutacao_possivel_espaco_aux3(N, Lista, [], Res),
    member(Perm, Res).

%numero espacos em comum =/= variaveis de esp
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    permutacao_possivel_aux6(Perms_soma, Esp, Perms_soma_Esp),
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    permutacoes_soma_espacos(Esps_com, Perms_soma_com),
    permutacao_possivel_aux4(Perms_soma_com, [], RetirarASoma), 
    permutacao_possivel_aux5(RetirarASoma, [], PermsEspCom), 
    nth1(1, Perms_soma_Esp, NVariaveis),
    length(NVariaveis, V),
    length(Esps_com, X),
    \+X = V,                                                             % numero de variaveis =/= numero de espacos comuns
    permutacao_possivel_espaco_aux7(X, Perms_soma_Esp, [], Permutacoes), % obter permutacoes mas com o length necessario
    permutacao_possivel_espaco_aux(Permutacoes, PermsEspCom, [], Lista),
    length(Perms_soma_com, N),
    permutacao_possivel_espaco_aux3(N, Lista, [], Res),
    sort(Res, Meio),
    member(Perm, Meio).

%aux para aux2
permutacao_possivel_espaco_aux([], _, Total, Total).
permutacao_possivel_espaco_aux([P|R], Q, Criar, Lista) :- 
    permutacao_possivel_espaco_aux2(P, Q, [], Resultado),
    append(Criar, [Resultado], Total),
    permutacao_possivel_espaco_aux(R, Q, Total, Lista), !.

%se pertence a perms
permutacao_possivel_espaco_aux2([], [], Total, Total).
permutacao_possivel_espaco_aux2([P|R], [Q|U], Fila, Resultado) :-  
    flatten(Q, Perms),
    membro(P, Perms),
    append(Fila, [P], Total),
    permutacao_possivel_espaco_aux2(R, U, Total, Resultado), !.

%se nao pertence a perms
permutacao_possivel_espaco_aux2([P|R], [Q|U], Fila, Lista) :-
    flatten(Q, Perms),
    \+membro(P, Perms),
    append(Fila, [], Total),
    permutacao_possivel_espaco_aux2(R, U, Total, Lista), !.

%se a length for o suposto damos append
permutacao_possivel_espaco_aux3(_, [], Total, Total).
permutacao_possivel_espaco_aux3(N, [P|Q], Criar, Perm) :-
    length(P, N),
    append(Criar, [P], Total),
    permutacao_possivel_espaco_aux3(N, Q, Total, Perm).

%se a length nao for o suposto nao damos append
permutacao_possivel_espaco_aux3(N, [P|Q], Criar, Perm) :-
    \+length(P, N),
    permutacao_possivel_espaco_aux3(N, Q, Criar, Perm).

% retirar a soma
permutacao_possivel_aux4([], Total, Total).
permutacao_possivel_aux4([P|Q], Fila, Lista) :-
    criar_espaco(Soma, _, Espaco),
    nth1(1, P, Espaco),
    nth1(2, P, Permutacoes),
    criar_perms(Espaco, Permutacoes, permutacoes(Espaco, Permutacoes)),
    append(Fila, [[Soma, Permutacoes]], Total),
    permutacao_possivel_aux4(Q, Total, Lista).

%mandar as permutacoes para uma lista
permutacao_possivel_aux5([], Total, Total).
permutacao_possivel_aux5([P|Q], Fila, Lista) :-
    nth1(2, P, Permutacoes),
    append(Fila, [Permutacoes], Total),
    permutacao_possivel_aux5(Q, Total, Lista).

permutacao_possivel_aux6([P|_], Esp, Res) :-
    %Perms_soma, Espaco, Res
    nth1(1, P, Espaco),
    Espaco == Esp,
    nth1(2, P, Res), !.

permutacao_possivel_aux6([P|Q], Esp, Res) :-
    %Perms_soma, Espaco, Res
    nth1(1, P, Espaco),
    Espaco \== Esp,
    permutacao_possivel_aux6(Q, Esp, Res).

permutacao_possivel_espaco_aux7(_, [], Total, Total).
permutacao_possivel_espaco_aux7(X, [P|Q], Fila, Lista) :-
    permutacoes(X, P, Add),
    append(Fila, Add, Total),
    permutacao_possivel_espaco_aux7(X, Q, Total, Lista), !.

% =============== 9 ===============

/*
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss).
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss),
em que Espacos e uma lista de espacos, Perms_soma e uma lista
de listas tal como obtida pelo predicado permutacoes_soma_espacos, e Esp e um
espaco, significa que Perms_poss e uma lista de 2 elementos em que o primeiro e a
lista de varioveis de Esp e o segundo e a lista ordenada de permutacoes possiveis para o
espaco Esp, tal como descrito na Seccao 2.1, no passo 2.

Sumario:
Perms_poss e a lista de dois elementos em que o primeiro e as variaveis de Esp
e o segundo elemento e as permutacoes possiveis dele
*/

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    criar_espaco(_, Vars, Esp),
    bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Lista),            %o conjunto de todos os permutacao_possivel_espaco
    Perms_poss = [Vars, Lista].

% =============== 10 ===============

/*
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps).
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps), em que
Espacos e uma lista de espacos, significa que Perms_poss_esps e a lista de permutacoes
possiveis, tal como descrito na Seccao 2.1, no passo 2.

Sumario:
Lista de listas em que cada elemento e um espaco de Espacos constituido pelas variaveis e
as permutacoes possiveis para o mesmo
*/

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    bagof(Lista,permutacoes_possiveis_espacos_aux2(Espacos, [], Lista),Perms_poss_esps).

permutacoes_possiveis_espacos_aux2(E, Fila, Lista) :-
    permutacoes_possiveis_espacos_aux(E,Res),
    nth1(1, Res, Res1),
    append(Fila, Res1, Lista).

permutacoes_possiveis_espacos_aux(Espacos, Perms_poss_esps) :-
    bagof(Perms_poss,(member(Espaco,Espacos),permutacoes_soma_espacos(Espacos,Perms_somas),permutacoes_possiveis_espaco(Espacos,Perms_somas,Espaco,Perms_poss)),Perms_poss_esps).


% =============== 11 ===============

/*
numeros_comuns(Lst_Perms, Numeros_comuns).
numeros_comuns(Lst_Perms, Numeros_comuns), em que Lst_Perms e uma lista
de permutacoes, significa que Numeros_comuns e uma lista de pares (pos, numero),
significando que todas as listas de Lst_Perms contem o numero numero na posicao
pos.

Sumario:
Encontra uma lista que e constituida por (Posicao, Numero) dos numeros iguais
da lista dada (Lst_Perms)
*/

numeros_comuns(Lst_Perms, Numeros_comuns) :-
    length(Lst_Perms, V),
    X is V-1,
    N is 1,
    nth1(N, Lst_Perms, PLista),
    tira(PLista, Lst_Perms, MeioLista),                             %podemos tirar a primeira lista sem unificar (delete)
    numeros_comuns_aux(PLista, MeioLista, [], Lista, N, X),
    Numeros_comuns = Lista, !.

numeros_comuns_aux([], _, Total, Total, _, _).
numeros_comuns_aux([P|Q], MeioLista, Fila, Lista, N, Tamanho) :-
    numeros_comuns_aux2(P, MeioLista, [], Res, N),
    length(Res, Tamanho),                                           %como aparece em todas as MeioLista vai ser em comum
    append(Fila, [(N, P)], Total),
    N1 is N+1,
    numeros_comuns_aux(Q, MeioLista, Total, Lista, N1, Tamanho).

numeros_comuns_aux([P|Q], MeioLista, Fila, Lista, N, Tamanho) :-
    numeros_comuns_aux2(P, MeioLista, [], Res, N),
    \+length(Res, Tamanho),                                         %significa que e preciso aparecer em todas as MeioLista para ser em comum
    N1 is N+1,
    numeros_comuns_aux(Q, MeioLista, Fila, Lista, N1, Tamanho).

numeros_comuns_aux2(_, [], Meio, Meio, _).
numeros_comuns_aux2(P, [R|U], Criar, Res, N) :-
    \+var(P),
    nth1(N, R, P),                                          %se estiver o numero certo na posicao certa, vai estar em comum
    append(Criar, [P] ,Meio),
    numeros_comuns_aux2(P, U, Meio, Res, N).

numeros_comuns_aux2(P, [R|U], Criar, Res, N) :-
    \+var(P),
    \+nth1(N, R, P),                                        %se n estiver o numero certo na posicao certa, nao vai estar em comum
    append(Criar, [] ,Meio),
    numeros_comuns_aux2(P, U, Meio, Res, N).

numeros_comuns_aux2(P, [_|U], Criar, Res, N) :-
    var(P),                                                 %se for uma var nao ha como comparar e n queremos unificar
    numeros_comuns_aux2(P, U, Criar, Res, N).

% =============== 12 ===============

/*
atribui_comuns(Perms_Possiveis).
atribui_comuns(Perms_Possiveis), em que Perms_Possiveis e uma lista de
permutacoes possiveis, actualiza esta lista atribuindo a cada espaco numeros comuns
a todas as permutacoes possiveis para esse espaco, tal como descrito na Seccao 2.1, no
passo 3a.

Sumario:
Encontra os numeros comuns das permutacoes de um espaco e mete esses numeros nas posicoes
corretas das variaveis
*/

criar_numero_comum(Posicao, Numero, (Posicao, Numero)).

atribui_comuns(Perms_Possiveis) :-
    atribui_comuns_aux(Perms_Possiveis, [], _), !.

atribui_comuns_aux([], Total, Total).
atribui_comuns_aux([P|Q], Fila, Lista) :-           % recebo perms_possiveis, uma lista vazia e uma lista a indefinida
    nth1(2, P, Permutacoes),                        % o segundo indice de P serao as permutacoes
    numeros_comuns(Permutacoes, N),                 % N serao os numeros comuns
    length(N, 0),                                   % se nao existem numeros comuns
    append(Fila, P, Total),                         % nao se fazem alteracoes
    atribui_comuns_aux(Q, Total, Lista).            %chamada recursiva da funcao

atribui_comuns_aux([P|Q], Fila, Lista) :-           % recebo perms_possiveis, uma lista vazia e uma lista a indefinida
    nth1(2, P, Permutacoes),                        % o segundo indice de P serao as permutacoes
    numeros_comuns(Permutacoes, Numeros_comuns),    % N serao os numeros comuns
    \+length(Numeros_comuns, 0),                    % existem numeros comuns
    atribui_comuns_aux2(P, Numeros_comuns, L),      % chamamos aux2 para podermos ter L e P 
    append(Fila, L, Total),                         
    atribui_comuns_aux(Q, Total, Lista).

atribui_comuns_aux2(_, [], _).
atribui_comuns_aux2(P, [R|U], L) :-
    nth1(1, P, Vars),
    nth1(2, P, Permutacoes),
    criar_numero_comum(Posicao, Numero, (R)),
    findall(X, select(_, Vars, Numero, X), Res),
    nth1(Posicao, Res, NewVars),
    P = [NewVars, Permutacoes],                     %P agora sera assim e podemos ter a nossa resposta gracas a isto
    atribui_comuns_aux2(P, U, L).

% =============== 13 ===============

/*
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis).
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis), em
que Perms_Possiveis e uma lista de permutacoes possiveis, significa que
Novas_Perms_Possiveis e o resultado de tirar permutacoes impossiveis de
Perms_Possiveis, tal como descrito na Seccao 2.1, no passo 3b.

Sumario:
Compara um a um as variaveis de um espaco com cada uma das permutacoes
possiveis para ver se os numeros que estao nas variaveis estao na mesma
posicao e tem o mesmo valor que essa permutacao.
Se tiverem, mantem-se na lista, se nao tiverem, saiem.
*/

retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    retira_impossiveis_aux(Perms_Possiveis, [], Novas_Perms_Possiveis), !.

retira_impossiveis_aux([], Total, Total).
retira_impossiveis_aux([P|Q], Fila, Novas_Perms_Possiveis) :-
    nth1(1, P, Vars),
    nth1(2, P, Perms),
    retira_impossiveis_aux2(Vars, [], Lista),
    length(Lista, N),                                               %numero de Numeros na parte esquerda de P
    retira_impossiveis_aux3(Vars, Perms, N, [], PermsEstaFila),     %encontrar as perms corretas na fila em especifico
    append(Fila, [[Vars, PermsEstaFila]], Total),                   %acrescentar a nova fila de perms possiveis
    retira_impossiveis_aux(Q, Total, Novas_Perms_Possiveis).

%encontrar as Perms que tem o mesmo numero de numeros comuns com as Vars, como as Vars tem de numeros
retira_impossiveis_aux3(_, [], _, Total, Total).
retira_impossiveis_aux3(Vars, [P|Q], N, Fila, Lista) :-
    append([Vars], [P], Comparar),                                  %fila de vars e P para ter os seus numeros em comum
    numeros_comuns(Comparar, NumCom),
    length(NumCom, X),
    X = N,                                                          %so se o numero de numeros em comum for igual ao numero de numeros e que ficam
    append(Fila, [P], Total),
    retira_impossiveis_aux3(Vars, Q, N, Total, Lista), !.

retira_impossiveis_aux3(Vars, [P|Q], N, Fila, Lista) :-
    append([Vars], [P], Comparar),
    numeros_comuns(Comparar, NumCom),
    length(NumCom, X),
    X \= N,                                                         %se n forem iguais ent nao queremos na nova lista
    retira_impossiveis_aux3(Vars, Q, N, Fila, Lista), !.

%encontrar o numero de Numeros na parte esquerda de cada Fila
retira_impossiveis_aux2([], Total, Total).
retira_impossiveis_aux2([P|Q], Fila, Lista):-
    var(P),                                                         %se e var nao contamos
    retira_impossiveis_aux2(Q, Fila, Lista), !.

retira_impossiveis_aux2([P|Q], Fila, Lista):-
    \+var(P),                                                       %se n e var podemos contar
    append(Fila, [P], Total),
    retira_impossiveis_aux2(Q, Total, Lista), !.


% =============== 14 ===============

/*
simplifica(Perms_Possiveis, Novas_Perms_Possiveis).
simplifica(Perms_Possiveis, Novas_Perms_Possiveis), em que
Perms_Possiveis e uma lista de permutacoes possiveis, significa que
Novas_Perms_Possiveis e o resultado de simplificar Perms_Possiveis , tal
como descrito na Seccao 2.1, no passo 3. Para simplificar uma lista de permutacoes
possiveis, deve aplicar-lhe os predicados atribui_comuns e retira_impossiveis,
por esta ordem, ate nao haver mais alteracoes.

Sumario:
E aplicado o predicado atribui comuns e retira_impossiveis a Perms_Possiveis
ate que ja nao haja nenhuma alteracao ao fazer isso
*/

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, X),
    simplifica_aux(Perms_Possiveis, X, Novas_Perms_Possiveis), !.

simplifica_aux(Perms_Possiveis, X, Lista) :-
    Perms_Possiveis == X,
    Lista = X.

simplifica_aux(Perms_Possiveis, X, Lista) :-
    Perms_Possiveis \== X,
    atribui_comuns(X),
    retira_impossiveis(X, Novas),
    simplifica_aux(X, Novas, Lista).

% =============== 15 ===============  

/*
inicializa(Puzzle, Perms_Possiveis).
inicializa(Puzzle, Perms_Possiveis), em que Puzzle e um puzzle, significa
que Perms_Possiveis e a lista de permutacoes possiveis simplificada para Puzzle.

Sumario:
Encontra as informacoes necessarias de um puzzle para podermos o simplificar
com o predicado simplifica
*/

inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis_Inicio),
    simplifica(Perms_Possiveis_Inicio, Perms_Possiveis).

% =============== 2.1 ===============

/*
escolhe_menos_alternativas(Perms_Possiveis, Escolha).
escolhe_menos_alternativas(Perms_Possiveis, Escolha), em que
Perms_Possiveis e uma lista de permutacoes possiveis, significa que Escolha
e o elemento de Perms_Possiveis escolhido segundo o criterio indicado na Seccao 2.2,
no passo 1. Se todos os espacos em Perms_Possiveis tiverem associadas listas de
permutacoes unitorias, o predicado deve devolver "falso".

Sumario:
Escolhe o espaco com menos permutacoes (mas mais que uma)
*/

escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    escolhe_menos_alternativas_aux(Perms_Possiveis, [], NPerms_Possiveis),
    \+length(NPerms_Possiveis, 0),
    escolhe_menos_alternativas_aux2(NPerms_Possiveis, [], Lista),       %lista de listas com o numero de perms como 1o elemento
    sort(Lista, Res),                                                   %sort da Lista
    escolhe_menos_alternativas_aux3(Res, [], X),                        %retirar o numero da Lista
    nth1(1, X, Escolha).

escolhe_menos_alternativas(Perms_Possiveis, _) :-
    escolhe_menos_alternativas_aux(Perms_Possiveis, [], NPerms_Possiveis),
    length(NPerms_Possiveis, 0),
    false.

%retirar todas as que tem so 1 permutacao
escolhe_menos_alternativas_aux([], Total, Total).
escolhe_menos_alternativas_aux([P|Q], Fila, Lista) :-
    nth1(2, P, Perms),
    length(Perms, 1),
    escolhe_menos_alternativas_aux(Q, Fila, Lista).

escolhe_menos_alternativas_aux([P|Q], Fila, Lista) :-
    nth1(2, P, Perms),
    \+length(Perms, 1),
    append(Fila, [P], Total),
    escolhe_menos_alternativas_aux(Q, Total, Lista).

%meter o numero de permutacoes como primeiro indice
escolhe_menos_alternativas_aux2([], Total, Total).
escolhe_menos_alternativas_aux2([P|Q], Fila, Lista) :-
    nth1(2, P, Perms),
    length(Perms, N),
    append(Fila, [[N, P]], Total),
    escolhe_menos_alternativas_aux2(Q, Total, Lista).

%retirar o numero de permutacoes
escolhe_menos_alternativas_aux3([], Total, Total).
escolhe_menos_alternativas_aux3([P|Q], Fila, Lista) :-
    nth1(2, P, Suposto),
    append(Fila, [Suposto], Total),
    escolhe_menos_alternativas_aux3(Q, Total, Lista).


% =============== 2.2 ===============

/*
A chamada experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis).
A chamada experimenta_perm(Escolha, Perms_Possiveis,
Novas_Perms_Possiveis), em que Perms_Possiveis e uma lista de permutacoes
possiveis, e Escolha e um dos seus elementos (escolhido pelo predicado
anterior), segue os seguintes passos:
1. Sendo Esp e Lst_Perms o espaco e a lista de permutacoes de
Escolha, respectivamente, escolhe uma permutacao de Lst_Perms,
Perm. Utilize o predicado member para escolher esta permutacao.
2. Unifica Esp com Perm.
3. Novas_Perms_Possiveis e o resultado de substituir, em Perms_Possiveis, o
elemento Escolha pelo elemento [Esp, [Perm]].
*/

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
    %Escolha e um elemento de uma lista de permutacoes possiveis
    %Perms_Possiveis e uma lista de permutacoes 
    %Escolha e constituiduo por Esp e Lst_Perms 
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, Lst_Perms),
    member(Perm, Lst_Perms),
    Esp = Perm,
    NovoElemento = [Esp, [Perm]],
    experimenta_perm_aux(NovoElemento, Escolha, Perms_Possiveis, Novas_Perms_Possiveis), !.

experimenta_perm_aux(_, _, [], []).
experimenta_perm_aux(NovoElemento, Escolha, [P|Q], [R|U]) :-
    P == Escolha,
    R = NovoElemento,
    experimenta_perm_aux(NovoElemento, Escolha, Q, U), !.

experimenta_perm_aux(NovoElemento, Escolha, [P|Q], [R|U]) :-
    P \== Escolha,
    R = P,
    experimenta_perm_aux(NovoElemento, Escolha, Q, U), !.