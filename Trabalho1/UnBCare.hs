module UnBCare where

import ModeloDados

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

 
 
O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}


{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

existeMedicamento :: Medicamento -> EstoqueMedicamentos -> Bool
existeMedicamento _ [] = False
existeMedicamento _med ((med,qnt):tail)
    | med == _med = True
    | otherwise = existeMedicamento _med tail

adicionarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
adicionarMedicamento _med qntIn ( (med,qnt) : tail )
    | _med == med = ( (med, qnt + qntIn) : tail)
    | otherwise = (med,qnt) : adicionarMedicamento _med qntIn tail

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qnt est 
    | est == [] = [(med, qnt)]
    | otherwise = do
        if existeMedicamento med est then adicionarMedicamento med qnt est 
        else (med, qnt):est


{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

consumir1Unidade :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
consumir1Unidade _med ((med,qnt):tail)
    | _med == med = ((med, qnt - 1) : tail)
    | otherwise = (med,qnt) : consumir1Unidade _med tail


quantidadeMedicamento :: Medicamento -> EstoqueMedicamentos -> Int
quantidadeMedicamento _med [] = 0
quantidadeMedicamento _med ( (med,qnt) : tail )
    | _med == med = qnt
    | otherwise = quantidadeMedicamento _med tail

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _med [] = Nothing
tomarMedicamento _med ((med, qnt):tail)
    | quantidadeMedicamento _med ((med, qnt):tail) > 0 = Just (consumir1Unidade _med ((med, qnt):tail))
    | otherwise = Nothing



{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento med est = quantidadeMedicamento med est

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

quickSort [] = []
quickSort (a:as) = quickSort [e | e <- as, e < a] ++ [a] ++ quickSort [e | e <- as, e >= a]

tamanhoLista [] = 0
tamanhoLista (num:tail) = 1 + tamanhoLista tail

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((med, hor):tail) = quickSort( (med, tamanhoLista hor) : demandaMedicamentos tail)

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

quickSort2 [] = []
quickSort2 (a:as) = quickSort2 [e | e <- as, e < a] ++ [a] ++ quickSort2 [e | e <- as, e > a]

pegaMedicamentos :: Receituario -> [String]
pegaMedicamentos [] = []
pegaMedicamentos ((med, hor):tail) = med : pegaMedicamentos tail

checkReceituario :: Receituario -> Bool
checkReceituario _receituario = pegaMedicamentos _receituario == quickSort2 (pegaMedicamentos _receituario)

checkHorario :: Receituario -> Bool
checkHorario [] = True
checkHorario ((med, hor):tail)
    | quickSort2 hor == hor = checkHorario tail
    | otherwise = False

receituarioValido :: Receituario -> Bool
receituarioValido rece = checkReceituario rece && checkHorario rece

checkMedPlano :: PlanoMedicamento -> Bool
checkMedPlano [] = True
checkMedPlano ((_hor, _meds):tail) 
    | quickSort2 _meds == _meds = checkMedPlano tail
    | otherwise = False

pegaHor :: PlanoMedicamento -> [Int]
pegaHor [] = []
pegaHor ((hor, med):tail) = hor : pegaHor tail

checkPlanoHorario :: PlanoMedicamento -> Bool
checkPlanoHorario _plano = quickSort2 (pegaHor _plano) == pegaHor _plano

planoValido :: PlanoMedicamento -> Bool
planoValido _plano = 
    and 
    [
        checkPlanoHorario _plano,
        checkMedPlano _plano
    ]


{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:
    
 -}


existe a [] = False
existe a (num:tail)
    | a == num = True
    | otherwise = existe a tail

checkRepetidos [] [] = False
checkRepetidos a [] = False
checkRepetidos [] b = False
checkRepetidos (num:tail) b
    | existe num b = True
    | otherwise = checkRepetidos tail b

pegaMedicar [] = []
pegaMedicar (acao:tail) = case acao of
    Medicar med -> med : pegaMedicar tail
    _ -> pegaMedicar tail

pegaComprar [] = []
pegaComprar (acao:tail) = case acao of
    Comprar med qnt -> med : pegaComprar tail
    _ -> pegaComprar tail

checkMedicar [] = True
checkMedicar ((hor, acao):tail)
    | pegaMedicar acao == quickSort2(pegaMedicar acao) = checkMedicar tail
    | otherwise = False

checkMedicarComprar [] = True
checkMedicarComprar ((hor, acao):tail)
    | checkRepetidos (pegaMedicar acao) (pegaComprar acao) = False
    | otherwise = checkMedicarComprar tail

pegaHorarios [] = []
pegaHorarios ((hor, acao):tail) = hor : pegaHorarios tail

checkHorarios _plantao = quickSort2 (pegaHorarios _plantao) == pegaHorarios _plantao
    
plantaoValido :: Plantao -> Bool
plantaoValido _plantao = 
    and
    [
        checkHorarios _plantao,
        checkMedicarComprar _plantao,
        checkMedicar _plantao
    ]


{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

adicionaNoPlano :: Int -> Medicamento -> PlanoMedicamento -> PlanoMedicamento
adicionaNoPlano _horario _medicamento ((hor, medicamentos) : tail)
    | _horario == hor = ( (hor, quickSort2(_medicamento : medicamentos) ) : tail )
    | otherwise = (hor, medicamentos) : ( adicionaNoPlano _horario _medicamento tail )

pegaHorariosReceituario :: Receituario -> [Int]
pegaHorariosReceituario [] = []
pegaHorariosReceituario ((_medicamento, _horarios): tail) = 
    quickSort2 (_horarios ++ pegaHorariosReceituario  tail)

pegaPlanoBase :: [Int] -> PlanoMedicamento
pegaPlanoBase [] = []
pegaPlanoBase (_horario: tail) =
    (_horario, []) : pegaPlanoBase tail

insereReceituarioNoPlano :: Receituario -> PlanoMedicamento -> PlanoMedicamento
insereReceituarioNoPlano [] _plano = _plano
insereReceituarioNoPlano ( ( _med, [] ) : tailReceituario ) _plano = insereReceituarioNoPlano tailReceituario _plano
insereReceituarioNoPlano ( ( _med, ( _hor : tailHorario ) ) : tailReceituario) _plano = do
    let _planoBase = adicionaNoPlano _hor _med _plano
    insereReceituarioNoPlano ( ( _med, tailHorario ) : tailReceituario ) _planoBase

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario _receituario = do
        let _horarioBase = pegaHorariosReceituario _receituario
        let _planoBase = pegaPlanoBase _horarioBase
        insereReceituarioNoPlano _receituario _planoBase

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

adicionaNoReceituario :: Medicamento -> Int -> Receituario -> Receituario
adicionaNoReceituario _medicamento _horario ((_med, _horarios) : tail)
    | _medicamento == _med = ( (_med, quickSort2(_horario : _horarios) ) : tail )
    | otherwise = (_med, _horarios) : ( adicionaNoReceituario _medicamento _horario tail )

pegaMedicamentosPlano :: [(Int, [String])] -> [String]
pegaMedicamentosPlano [] = []
pegaMedicamentosPlano ((_hor, _medicamentos):tail) = 
    quickSort2(_medicamentos ++ pegaMedicamentosPlano tail)

pegaReceituarioBase :: [String] -> Receituario
pegaReceituarioBase [] = []
pegaReceituarioBase (_medicamento:tail) =
    (_medicamento, []) : pegaReceituarioBase tail

inserePlanoNoReceituario :: PlanoMedicamento -> Receituario -> Receituario
inserePlanoNoReceituario [] _receituario = _receituario
inserePlanoNoReceituario ( ( _horario, [] ) : tailPlano ) _receituario = inserePlanoNoReceituario tailPlano _receituario
inserePlanoNoReceituario ( ( _horario, ( _medicamento : tailMedicamento ) ) : tailPlano) _receituario = do
    let _receituarioBase = adicionaNoReceituario _medicamento _horario _receituario
    inserePlanoNoReceituario ( ( _horario, tailMedicamento ) : tailPlano ) _receituarioBase

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano _plano = do
    let _medicamentosBase = pegaMedicamentosPlano _plano
    let _receituarioBase = pegaReceituarioBase _medicamentosBase
    inserePlanoNoReceituario _plano _receituarioBase


{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado 
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

addMedicamento :: Medicamento -> Int -> EstoqueMedicamentos -> EstoqueMedicamentos
addMedicamento _med _qnt [] = [(_med, _qnt)]
addMedicamento _medicamento _val ((_med, _qnt):tail)
    | _medicamento == _med = ( (_med, _qnt + _val) : tail )
    | otherwise = ( (_med, _qnt) : addMedicamento _medicamento _val tail)

checkEstoque :: EstoqueMedicamentos -> Bool
checkEstoque [] = True
checkEstoque ( ( _med, _qnt ) : tail ) = _qnt >= 0 && checkEstoque tail

executaPlantaoAux :: Plantao -> EstoqueMedicamentos -> EstoqueMedicamentos
executaPlantaoAux [] _estoque = _estoque
executaPlantaoAux ( ( _horario, [] ) : tailPlantao ) _estoque = (executaPlantaoAux tailPlantao _estoque)
executaPlantaoAux ( ( _horario, ( acao : tailAcao ) ) : tailPlantao) _estoque = case acao of 
    Medicar _med -> do
        let _novoEstoque = (addMedicamento _med (-1) _estoque)
        if checkEstoque _novoEstoque then executaPlantaoAux ( ( _horario, tailAcao  ) : tailPlantao) _novoEstoque
        else [("Invalido", -1)]
    Comprar _med _qnt -> do
        let _novoEstoque = (addMedicamento _med _qnt _estoque)
        if checkEstoque _novoEstoque then executaPlantaoAux ( ( _horario, tailAcao  ) : tailPlantao) _novoEstoque
        else [("Invalido", -1)]

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao _plantao _estoque = do
    let result = executaPlantaoAux _plantao _estoque
    if result == [("Invalido", -1)] then Nothing
    else Just result

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano 
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão 
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

plantaoPlano :: Plantao -> PlanoMedicamento
plantaoPlano [] = []
plantaoPlano ( (_horario, _acao) : tail ) = do
    let medicar = pegaMedicar _acao
    if medicar == [] then plantaoPlano tail
    else  ( _horario, medicar ) : plantaoPlano tail

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos  -> Bool
satisfaz _plantao _plano _estoque = 
    and[
        not (executaPlantao _plantao _estoque == Nothing),
        plantaoPlano _plantao == _plano
    ]

{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

executaMedicar :: [Cuidado] -> EstoqueMedicamentos -> EstoqueMedicamentos
executaMedicar [] _estoque = _estoque
executaMedicar (_meds:tail) _estoque = case _meds of
    Medicar _meds -> executaMedicar tail (addMedicamento _meds (-1) _estoque)

criaListaMedicar :: [Medicamento] -> [Cuidado]
criaListaMedicar [] = []
criaListaMedicar (med : tail) = Medicar med : criaListaMedicar tail

criaListaComprar :: EstoqueMedicamentos -> [Cuidado]
criaListaComprar [] = []
criaListaComprar ( (_med , qnt) : tail )
    | qnt >= 0 = criaListaComprar tail
    | otherwise = (Comprar _med (-qnt)) : criaListaComprar tail

concertaEstoque :: EstoqueMedicamentos -> EstoqueMedicamentos
concertaEstoque [] = []
concertaEstoque ( (_med , qnt) : tail )
    | qnt >= 0 = (_med , qnt) : concertaEstoque tail
    | otherwise = (_med , 0) : concertaEstoque tail

geraComprar :: PlanoMedicamento ->  EstoqueMedicamentos -> [Cuidado]
geraComprar [] _ = []
geraComprar ((_hor, _meds):tail)  _estoque = do
    let _acaoMedicar = criaListaMedicar _meds
    let _estoqueLinha = executaMedicar _acaoMedicar []
    let _estoqueAux = executaMedicar _acaoMedicar _estoque
    let _acaoComprar = criaListaComprar _estoqueAux
    let _estoqueArrumado = concertaEstoque _estoqueAux
    _acaoComprar ++ (geraComprar tail  _estoqueArrumado)

executaPlantaoMeds :: PlanoMedicamento -> Plantao
executaPlantaoMeds [] = []
executaPlantaoMeds ((_hor, _meds):tail) = do
    let _acaoMedicar = criaListaMedicar _meds
    (_hor, _acaoMedicar) : executaPlantaoMeds tail


plantaoCorreto :: PlanoMedicamento ->  EstoqueMedicamentos  -> Plantao
plantaoCorreto [] _ = []
plantaoCorreto _plano  _estoque = do
    let baseMedicar = executaPlantaoMeds _plano
    let baseComprar = geraComprar _plano _estoque
    if tamanhoLista baseComprar == 0 then baseMedicar
    else (0, baseComprar) : baseMedicar