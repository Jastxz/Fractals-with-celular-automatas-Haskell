module Automata(
  propiedadesRegla,
  aplicaRegla,
) where

import Data.Matrix
import Tipos
import Utiles

numcelulas :: Int
numcelulas = 50

propiedadesRegla :: Int -> [String]
propiedadesRegla regla
  | numeroRegla regla == 30 = [senConIni, topTrans, punDen]
  | numeroRegla regla == 90 = [dimFractal]
  | numeroRegla regla == 150 = [senConIni, topTrans, punDen]
  | otherwise = error $ "A la funcion propiedadesRegla le entra una regla no valida. La regla: " ++ show regla 
  where
    senConIni = "Sensitivity to initial conditions"
    topTrans = "Topologically transitive"
    punDen = "Periodic points are dense"
    dimFractal = "Fractal dimension"

numeroRegla :: Int -> Int
numeroRegla r 
  | r==0 = 30
  | r==1 = 90
  | r==2 = 150
  | otherwise = error $ "No se reconoce el valor introducido para traducirlo a regla. El valor: " ++ show r

aplicaRegla :: Int -> Automata -> Automata
aplicaRegla regla automata = aplicaRegla' regla automata (base, base) nuevoAutomata
  where 
    (base, tam) = rangos automata
    nuevoAutomata = Data.Matrix.zero tam tam

aplicaRegla' :: Int -> Automata -> Pos -> Automata -> Automata
aplicaRegla' regla automata pos@(f,c) nuevoAutomata
  | f>tam = nuevoAutomata
  | c==tam = aplicaRegla' regla automata (f+1,base) na 
  | otherwise = aplicaRegla' regla automata (f,c+1) na
  where
    (base, tam) = rangos automata
    central = automata ! pos
    iz
      | c==base = automata ! (f,tam)
      | otherwise = automata ! (f,c-1)
    der
      | c==tam = automata ! (f,base)
      | otherwise = automata ! (f,c+1)
    ne 
      | regla==30 = regla30 iz central der
      | regla==90 = regla90 iz central der
      | regla==150 = regla150 iz central der
      | otherwise = error $ "A la funcion aplicaRegla le entra una regla no valida. La regla: " ++ show regla 
    na = Data.Matrix.setElem ne pos nuevoAutomata

regla30 :: Int -> Int -> Int -> Int
regla30 izq central der
  | evaluaEntorno izq central der == "100" = 1  
  | evaluaEntorno izq central der == "011" = 1   
  | evaluaEntorno izq central der == "010" = 1   
  | evaluaEntorno izq central der == "001" = 1   
  | otherwise = 0

regla90 :: Int -> Int -> Int -> Int
regla90 izq central der
  | evaluaEntorno izq central der == "100" = 1  
  | evaluaEntorno izq central der == "010" = 1   
  | evaluaEntorno izq central der == "001" = 1  
  | otherwise = 0

regla150 :: Int -> Int -> Int -> Int
regla150 izq central der
  | evaluaEntorno izq central der == "111" = 1
  | evaluaEntorno izq central der == "100" = 1 
  | evaluaEntorno izq central der == "010" = 1 
  | evaluaEntorno izq central der == "001" = 1
  | otherwise = 0

evaluaEntorno :: Int -> Int -> Int -> String
evaluaEntorno 1 1 1 = "111"
evaluaEntorno 1 1 0 = "110"
evaluaEntorno 1 0 1 = "101"
evaluaEntorno 1 0 0 = "100"
evaluaEntorno 0 1 1 = "011"
evaluaEntorno 0 1 0 = "010"
evaluaEntorno 0 0 1 = "001"
evaluaEntorno 0 0 0 = "000"
