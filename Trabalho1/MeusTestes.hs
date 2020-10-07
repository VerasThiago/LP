module MeusTestes where

import Exemplos
import ModeloDados
import UnBCare

{- 
m1 = "Lactulona"
m2 = "Pantoprazol"
m3 = "Patz"
m4 = "Quetiapina"
m5 = "Mirtazapina"
m6 = "Adera"
m7 = "Donepezila"
m8 = "Xarelto"
m9 = "Alprazolam"

estoque :: EstoqueMedicamentos
estoque1 = [(m1,10), (m2,5), (m3,0)]
estoque2 = [(m1,10), (m2,5), (m3,10)]
estoque3 = [(m1,10), (m2,50), (m3,10), (m4, 20)]

receituario1 :: Receituario
receituario1 = [(m1,[8,17]),(m2,[6]),(m3,[22])]

-}

mA :: Medicamento
mA = "A"

mB :: Medicamento
mB = "B"

mC :: Medicamento
mC = "C"

mD :: Medicamento
mD = "D"

mE :: Medicamento
mE = "E"

mF :: Medicamento
mF = "F"

estoqueA :: EstoqueMedicamentos
estoqueA = []

estoqueB :: EstoqueMedicamentos
estoqueB = [(mA, 0), (mB, 0)]

estoqueC :: EstoqueMedicamentos
estoqueC = [(mA, 4), (mB, 4), (mC, 4), (mD, 4)]

estoqueD :: EstoqueMedicamentos
estoqueD = [(mA, 3), (mB, 3), (mC, 3), (mD, 3)]

estoqueE :: EstoqueMedicamentos
estoqueE = [(mA, 4), (mB, 4), (mC, 4), (mD, 3)]

receituarioA :: Receituario
receituarioA = [(mA,[1,2,3,4,5,6,7,8,9,10]),(mB,[5]),(mC,[1])]

receituarioB :: Receituario
receituarioB = [(mB,[1,2,3,4,10]),(mC,[1]), (mD,[1,2,3,4,5,6,7,8,9,10])]

receituarioInvalidoA :: Receituario
receituarioInvalidoA = [(mA,[1,2,3,4,5,6,7,8,9,10]),(mA,[]),(mC,[1])]

receituarioInvalidoB :: Receituario
receituarioInvalidoB = [(mA,[1,2,3,4,5,6,7,8,9,10]),(mC,[]),(mB,[1])]

receituarioInvalidoC :: Receituario
receituarioInvalidoC = [(mA,[1,2,3,4,4,6,7,8,9,10]),(mB,[]),(mC,[1])]

receituarioInvalidoD :: Receituario
receituarioInvalidoD = [(mA,[1,2,3,5,4,6,7,8,9,10]),(mB,[]),(mC,[1])]

planoA :: PlanoMedicamento
planoA = [(6,[mB]),(8,[mA]),(17,[mA]),(22,[mC])]

planoB :: PlanoMedicamento
planoB = [(6,[mA, mB, mC, mD]),(8,[mA, mB, mC, mD]),(17,[mA, mB, mC, mD]),(22,[mA, mB, mC, mD])]

planoC :: PlanoMedicamento
planoC = [(6,[mA, mB, mC, mD]),(8,[mA, mB, mC, mD]),(17,[mA, mB, mC, mD]),(22,[mA, mB, mC, mD])]

planoD :: PlanoMedicamento
planoD = []

planoInvalidoA :: PlanoMedicamento
planoInvalidoA = [(1,[mA]),(1,[mB,mC]),(3,[mD]),(4,[mE,mF])]

planoInvalidoB :: PlanoMedicamento
planoInvalidoB = [(2,[mA]),(1,[mB,mC]),(3,[mD]),(4,[mE,mF])]

planoInvalidoC :: PlanoMedicamento
planoInvalidoC = [(1,[mA]),(2,[mC,mC]),(3,[mD]),(4,[mE,mF])]

planoInvalidoD :: PlanoMedicamento
planoInvalidoD = [(1,[mA]),(1,[mC,mB]),(3,[mD]),(4,[mE,mF])]


plantaoA :: Plantao
plantaoA = [(6,[Medicar mB])
           ,(8,[Medicar mA])
           ,(17,[Medicar mA, Comprar mC 30])
           ,(22,[Medicar mC])]

plantaoB :: Plantao
plantaoB = [(6,[Medicar mA, Medicar mB, Medicar mC, Medicar mD])
           ,(8,[Medicar mA, Medicar mB, Medicar mC, Medicar mD])
           ,(17,[Medicar mA, Medicar mB, Medicar mC, Medicar mD])
           ,(22,[Medicar mA, Medicar mB, Medicar mC, Medicar mD])]

plantaoC :: Plantao
plantaoC = [(5,[Comprar mA 4, Comprar mB 4, Comprar mC 4, Comprar mD 4])
           ,(6,[Medicar mA, Medicar mB, Medicar mC, Medicar mD])
           ,(8,[Medicar mA, Medicar mB, Medicar mC, Medicar mD])
           ,(17,[Medicar mA, Medicar mB, Medicar mC, Medicar mD])
           ,(22,[Medicar mA, Medicar mB, Medicar mC, Medicar mD])]

plantaoD :: Plantao
plantaoD = [(5,[Comprar mA 4, Comprar mB 4, Comprar mC 4, Comprar mD 4])]

plantaoInvalidoA :: Plantao
plantaoInvalidoA = [(6,[Medicar mB])
           ,(6,[Medicar mA])
           ,(17,[Medicar mA, Comprar mC 30])
           ,(22,[Medicar mC])]

plantaoInvalidoB :: Plantao
plantaoInvalidoB = [(8,[Medicar mB])
           ,(6,[Medicar mA])
           ,(17,[Medicar mA, Comprar mC 30])
           ,(22,[Medicar mC])]

plantaoInvalidoC :: Plantao
plantaoInvalidoC = [(6,[Medicar mB, Medicar mB])
           ,(8,[Medicar mA])
           ,(17,[Medicar mA, Comprar mC 30])
           ,(22,[Medicar mC])]

plantaoInvalidoD :: Plantao
plantaoInvalidoD = [(6,[Medicar mB])
           ,(8,[Medicar mA])
           ,(17,[Medicar mA, Comprar mC 30])
           ,(22,[Medicar mC, Medicar mC])]

plantaoInvalidoE :: Plantao
plantaoInvalidoE = [(6,[Medicar mB])
           ,(8,[Medicar mA])
           ,(17,[Medicar mA, Comprar mC 30])
           ,(22,[Medicar mC, Medicar mA])]

plantaoInvalidoF :: Plantao
plantaoInvalidoF = [(6,[Medicar mB])
           ,(8,[Medicar mA])
           ,(17,[Medicar mD, Comprar mC 30, Medicar mD])
           ,(22,[Medicar mC, Medicar mD])]

plantaoInvalidoG :: Plantao
plantaoInvalidoG = [(6,[Medicar mB])
           ,(8,[Medicar mA])
           ,(17,[Medicar mD, Comprar mD 30])
           ,(22,[Medicar mC, Medicar mD])]

plantaoInvalidoH :: Plantao
plantaoInvalidoH = [(6,[Medicar mB])
           ,(8,[Medicar mA])
           ,(17,[Medicar mA, Medicar mB, Medicar mC, Medicar mD, Comprar mA 30])
           ,(22,[Medicar mC, Medicar mD])]

plantaoInvalidoI :: Plantao
plantaoInvalidoI = [(6,[Medicar mB])
           ,(8,[Medicar mA])
           ,(16,[Medicar mC, Medicar mD])
           ,(17,[Medicar mA, Medicar mB, Medicar mC, Medicar mD, Comprar mA 30])]


-- QUESTAO 1
caso1 = comprarMedicamento mC 30 estoqueA == [(mC,30)]
caso2 = comprarMedicamento mC 30 estoqueB == [(mC,30), (mA, 0), (mB, 0)]
conjuntoCasos1 = and [caso1,caso2]

-- QUESTAO 2
caso3 = tomarMedicamento m1 estoqueA == Nothing
caso4 = tomarMedicamento m2 estoqueB == Nothing
caso5 = tomarMedicamento m3 estoqueB == Nothing
conjuntoCasos2 = and [caso3, caso4, caso5]

-- QUESTAO 3
caso6 = consultarMedicamento m2 estoqueA == 0
caso7 = consultarMedicamento m2 estoqueB == 0
caso8 = consultarMedicamento "Quetiapina" estoque3 == 20
conjuntoCasos3 = and [caso6, caso7, caso8]

-- QUESTAO 4
caso9 = demandaMedicamentos receituarioA == [(mA,10),(mB,1),(mC,1)]
caso10 = demandaMedicamentos receituarioB == [(mB,5),(mC,1),(mD,10)]
conjuntoCasos4 = and [caso9, caso10]

-- QUESTAO 5
caso11 = receituarioValido receituarioA == True
caso12 = receituarioValido receituarioInvalidoA == False
caso13 = receituarioValido receituarioInvalidoB == False
caso14 = receituarioValido receituarioInvalidoC == False
caso15 = receituarioValido receituarioInvalidoD == False
caso16 = planoValido planoA == True
caso17 = planoValido planoInvalidoA == False
caso18 = planoValido planoInvalidoB == False
caso19 = planoValido planoInvalidoC == False
caso20 = planoValido planoInvalidoD == False
caso1Extra = planoValido planoB == True
caso2Extra = planoValido planoC == True
caso3Extra = planoValido planoD == True
conjuntoCasos5 = and [caso11, caso12, caso13, caso14, caso15, caso16, caso17, caso18, caso19, caso20, caso2Extra, caso2Extra, caso3Extra]

-- QUESTAO 6
conjuntoCasos6 =
  and [ plantaoValido plantaoA == True
      , plantaoValido plantaoInvalidoA == False
      , plantaoValido plantaoInvalidoB == False
      , plantaoValido plantaoInvalidoC == False
      , plantaoValido plantaoInvalidoD == False
      , plantaoValido plantaoInvalidoE == False
      , plantaoValido plantaoInvalidoF == False
      , plantaoValido plantaoInvalidoG == False
      , plantaoValido plantaoInvalidoH == False
      , plantaoValido plantaoInvalidoI == False
      ]


-- QUESTAO 7
caso21 = geraPlanoReceituario receituarioA == [(1,[mA, mC]), (2,[mA]), (3,[mA]), (4,[mA]), (5,[mA, mB]), (6,[mA]), (7,[mA]), (8,[mA]), (9,[mA]), (10,[mA])]
caso22 = geraPlanoReceituario receituarioB == [(1,[mB, mC, mD]), (2,[mB, mD]), (3,[mB, mD]), (4,[mB, mD]), (5,[mD]), (6,[mD]), (7,[mD]), (8,[mD]), (9,[mD]), (10,[mB, mD])]
conjuntoCasos7 = and [caso21, caso22]

-- QUESTAO 8
caso24 = geraReceituarioPlano (geraPlanoReceituario receituarioA) == receituarioA
caso25 = geraReceituarioPlano (geraPlanoReceituario receituarioB) == receituarioB
conjuntoCasos8 = and [caso24,caso25]

-- QUESTAO 9
caso26 = executaPlantao plantaoA estoqueA == Nothing
caso27 = executaPlantao plantaoD estoqueA == Just [(mA, 4), (mB, 4), (mC, 4), (mD, 4)]
caso28 = executaPlantao plantaoA estoqueB == Nothing
caso29 = executaPlantao plantaoA estoqueC == Just [(mA, 2), (mB, 3), (mC, 33), (mD, 4)]
caso30 = executaPlantao plantaoB estoqueB == Nothing
caso31 = executaPlantao plantaoB estoqueC == Just [(mA, 0), (mB, 0), (mC, 0), (mD, 0)]
caso32 = executaPlantao plantaoB estoqueD == Nothing
caso33 = executaPlantao plantaoB estoqueE == Nothing
caso34 = executaPlantao plantaoC estoqueA == Just [(mA, 0), (mB, 0), (mC, 0), (mD, 0)]
caso35 = executaPlantao plantaoC estoqueB == Just [(mA, 0), (mB, 0), (mC, 0), (mD, 0)]
caso36 = executaPlantao plantaoC estoqueC == Just [(mA, 4), (mB, 4), (mC, 4), (mD, 4)]
caso37 = executaPlantao plantaoD estoqueC == Just [(mA, 8), (mB, 8), (mC, 8), (mD, 8)]
caso38 = executaPlantao plantaoD estoqueE == Just [(mA, 8), (mB, 8), (mC, 8), (mD, 7)]
conjuntoCasos9 = and [caso26, caso27, caso28, caso29, caso30, caso31,
                      caso32, caso33, caso34, caso35, caso36, caso37,
                      caso38]

-- QUESTAO 10
caso39 = satisfaz plantaoA planoA estoqueB == False
caso40 = satisfaz plantaoA planoA estoqueC == True
caso41 = satisfaz plantaoA planoA estoqueD == True

caso42 = satisfaz plantaoB planoB estoqueA == False
caso43 = satisfaz plantaoB planoB estoqueB == False
caso44 = satisfaz plantaoB planoB estoqueC == True
caso45 = satisfaz plantaoB planoB estoqueD == False

caso46 = satisfaz plantaoC planoC estoqueA == True
caso47 = satisfaz plantaoC planoC estoqueB == True
caso48 = satisfaz plantaoC planoC estoqueC == True
caso49 = satisfaz plantaoC planoC estoqueD == True

caso50 = satisfaz plantaoD planoD estoqueA == True
caso51 = satisfaz plantaoD planoD estoqueB == True
caso52 = satisfaz plantaoD planoD estoqueC == True
caso53 = satisfaz plantaoD planoD estoqueD == True

caso54 = satisfaz plantaoB planoD estoqueC == False
caso55 = satisfaz plantaoC planoD estoqueC == False
caso56 = satisfaz plantaoC planoB estoqueC == True
caso57 = satisfaz plantaoB planoC estoqueC == True
caso58 = satisfaz plantaoA planoA estoqueA == False

conjuntoCasos10 = and [caso39, caso40, caso41, caso42, caso43, caso44, caso45, caso46, caso47, 
                       caso48, caso49, caso50, caso51, caso52, caso53, caso54, caso55, caso56, caso57, caso58]

resultadoTestes = and [ resultadoGlobalTestes, conjuntoCasos1, conjuntoCasos2, conjuntoCasos3, conjuntoCasos4, conjuntoCasos5, conjuntoCasos6, conjuntoCasos7,
                        conjuntoCasos8, conjuntoCasos9, conjuntoCasos10 ]