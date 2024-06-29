module Tipos.TipoAutomata
  ( Automata,
    automataVacio,
    cuentaAparicionesPorFila,
    convertirListaEnAutomata,
    convertirListaDeListasEnAutomata,
    convertirJsonEnAutomata,
    convertirAutomataEnLista,
    convertirAutomataEnListaDelistas,
    convertirAutomataEnJson,
    elementoPorPosicion,
    intercambiaElementoPorPosicion,
    intercambiaElementoEnVector,
    convertirListaDeListasEnJson,
    convertirJsonEnListaDeListas,
  )
where

import Data.Aeson as A
import Data.ByteString.Lazy as B
import Data.StorableVector as V
import Foreign (Storable)
import Tipos.TipoPosicion
import Utilidades.Utiles
import Prelude as P

type Automata = [Vector Int]

automataVacio :: Automata
automataVacio = [V.singleton 1]

cuentaAparicionesPorFila :: Automata -> Int -> Int -> Int
cuentaAparicionesPorFila aut fila numero
  | V.elem numero vector = cuentaAparicionesEnVector vector numero
  | otherwise = 0
  where
    vector = posicion "cuentaAparicionesPorFila" fila aut

cuentaAparicionesEnVector :: V.Vector Int -> Int -> Int
cuentaAparicionesEnVector vector numero
  | V.null vector = 0
  | v == numero = 1 + acumulado
  | otherwise = acumulado
  where
    v = V.head vector
    resto = V.tail vector
    acumulado = cuentaAparicionesEnVector resto numero

convertirListaEnAutomata :: [Int] -> Int -> Automata
convertirListaEnAutomata listaDeNumeros tamañoPorFila = convertirListaDeListasEnAutomata $ agruparListaPorTamaño listaDeNumeros tamañoPorFila

convertirListaDeListasEnAutomata :: [[Int]] -> Automata
convertirListaDeListasEnAutomata = P.map V.pack

convertirJsonEnAutomata :: ByteString -> Automata
convertirJsonEnAutomata = convertirListaDeListasEnAutomata . convertirJsonEnListaDeListas

convertirAutomataEnLista :: Automata -> [Int]
convertirAutomataEnLista = P.concatMap V.unpack

convertirAutomataEnListaDelistas :: Automata -> [[Int]]
convertirAutomataEnListaDelistas = P.map V.unpack

convertirAutomataEnJson :: Automata -> ByteString
convertirAutomataEnJson = convertirListaDeListasEnJson . convertirAutomataEnListaDelistas

elementoPorPosicion :: Automata -> Pos -> Int
elementoPorPosicion automata (fila, columna) = V.index (indiceLista automata fila "elementoPorPosicion") columna

intercambiaElementoPorPosicion :: Automata -> Pos -> Int -> Automata
intercambiaElementoPorPosicion automata (fila, columna) elemento = intercambia automata nuevoVector fila
  where
    vector = indiceLista automata fila "intercambiaElementoPorPosicion"
    nuevoVector = intercambiaElementoEnVector vector columna elemento

intercambiaElementoEnVector :: (Storable a) => Vector a -> Int -> a -> Vector a
intercambiaElementoEnVector vector indice elemento = V.append principio $ V.cons elemento final
  where
    nUltimos = if indice == V.length vector then V.length vector else indice + 1
    principio = V.take indice vector
    final = V.drop nUltimos vector

convertirListaDeListasEnJson :: [[Int]] -> ByteString
convertirListaDeListasEnJson lista = A.encode [(indice, posicion "convertirListaDeListasEnJson" indice lista) | indice <- [0 .. P.length lista - 1]]

convertirJsonEnListaDeListas :: ByteString -> [[Int]]
convertirJsonEnListaDeListas bytes = P.map P.snd lista
  where
    may = A.decode bytes :: Maybe [(Int, [Int])]
    mensajeError = "Error al decodificar los bytes en la funcion convertirJsonEnListaDeListas. Json: " ++ show bytes
    lista = desempaquetaMaybe may mensajeError

desempaquetaMaybe :: P.Maybe a -> String -> a
desempaquetaMaybe (P.Just a) _ = a
desempaquetaMaybe P.Nothing mensaje = error mensaje
