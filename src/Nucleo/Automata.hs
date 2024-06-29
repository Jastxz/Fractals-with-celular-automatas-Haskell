module Nucleo.Automata
  ( propiedadesRegla,
    automataRandom,
    automataPreparado,
    aplicaRegla,
  )
where

import Prelude as P
import Data.StorableVector as V
import Tipos.TipoAutomata
import Tipos.TipoPosicion
import Utilidades.Utiles

propiedadesRegla :: Int -> [String]
propiedadesRegla regla = case regla of
  30 -> [senConIni, topTrans, punDen]
  73 -> [dimFractal++" ~1.7771.", boxC1, boxC2]
  90 -> [dimFractal++" ~1.5401.", boxC1, boxC2]
  105 -> [dimFractal++" ~1.7751.", boxC1, boxC2]
  122 -> [efecto]
  124 -> [dimFractal++" ~1.778.", boxC1, boxC2]
  126 -> [variacion++" ~1.6657.", boxC1, boxC2]
  150 -> [dimFractal++" ~1.6407.", boxC1, boxC2]
  193 -> [dimFractal++" ~1.7598.", boxC1, boxC2]
  195 -> [variacion, dimFractal++" ~1.5708.", boxC1, boxC2]
  _ -> ["No data."]
  where
    senConIni = "Sensitivity to initial conditions"
    topTrans = "Topologically transitive"
    punDen = "Periodic points are dense"
    dimFractal = "Fractal dimension: "
    efecto = "Simply is an optic effect"
    variacion = "Sierpinski triangle variation"
    boxC1 = "Note: Calculated with box counting method"
    boxC2 = " and an error of ~|0.05|."

automataRandom :: Int -> Int -> Automata
automataRandom semilla celulas
  | celulas <= 0 = error "Error en automataRandom. Se han indicado 0 o menos celulas"
  | otherwise = convertirListaEnAutomata listaAl celulas
    where
      mitad = celulas `div` 2
      listaAleatorios = P.take (celulas ^ 2) $ generaAleatorios semilla
      listaAl = P.map binario listaAleatorios

automataPreparado :: Int -> Automata
automataPreparado celulas
  | celulas <= 0 = error "Error en automataPreparado. Se han indicado 0 o menos celulas"
  | otherwise =  convertirListaEnAutomata listaPreparada celulas
    where
      centro = celulas `div` 2
      listaDeCeros = P.replicate (celulas ^ 2) 0
      listaPreparada = intercambia listaDeCeros 1 centro

aplicaRegla :: Int -> Int -> Automata -> Automata
aplicaRegla fila regla automata
  | fila == 0 = automata
  | fila > P.length automata - 1 = automata
  | otherwise = intercambia automata filaActualizada fila
    where
      vectorCels = posicion "aplicaRegla" fila automata
      vectorAnterior
        | fila == 0 = vectorCels
        | otherwise = posicion "aplicaRegla" (fila - 1) automata
      filaActualizada = aplicaRegla' regla vectorAnterior vectorCels (V.length vectorAnterior - 1, 0, 1)

aplicaRegla' :: Int -> Vector Int -> Vector Int -> (Int, Int, Int) -> Vector Int
aplicaRegla' regla vAnt vCels (iz, central, der)
  | central == tam = nuevoVector
  | otherwise = aplicaRegla' regla vAnt nuevoVector (niz, central + 1, nder)
  where
    tam = V.length vAnt - 1
    valorIz = V.index vAnt iz
    valorCentro = V.index vAnt central
    valorDer = V.index vAnt der
    nuevoValor = (regla `div` (2^(4*valorIz+2*valorCentro+valorDer))) `mod` 2
    nuevoVector = intercambiaElementoEnVector vCels central nuevoValor
    niz = if iz == tam then 0 else iz + 1
    nder = if der == tam then 0 else der + 1