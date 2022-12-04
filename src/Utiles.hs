module Utiles
  ( cabeza,
    esInt,
    esReal,
    stringToInt,
    stringToDouble,
    stringToFloat,
    listasDePares,
    distanciaEuclidea,
    intercambia,
    introduce,
    elimina,
    eliminaElemento,
    aleatorio,
    escogeAleatorios,
    normaliza,
    now,
    time,
    vacia,
  )
where

import Data.Matrix
import Data.Char
import qualified Data.Functor
import Data.Time.Clock
import Tipos

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones normales en útiles.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

cabeza :: String -> [a] -> a
cabeza funcion lista
  | null lista = error $ "La lista pasada en la funcion " ++ funcion ++ " esta vacia."
  | otherwise = head lista

esInt :: String -> Bool
esInt = foldr ((&&) . isDigit) True

esReal :: String -> Bool
esReal s
  | esInt entera && esInt fraccionaria = True
  | otherwise = False
  where
    entera = takeWhile (/= '.') s
    f = dropWhile (/= '.') s
    fraccionaria = if null f then "" else tail f

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt c@(s : ss)
  | s == '-' = (-1) * stringToInt ss
  | otherwise = (digitToInt s * 10 ^ (length c - 1)) + stringToInt ss

stringToDouble :: String -> Double
stringToDouble [] = 0.0
stringToDouble s = entera + fraccionaria
  where
    e = stringToInt $ takeWhile (/= '.') s
    entera = fromInteger $ toInteger e
    f = tail $ dropWhile (/= '.') s
    fracEnt = fromInteger $ toInteger $ stringToInt f
    fraccionaria = fracEnt / (10 ^ length f)

stringToFloat :: String -> Float
stringToFloat [] = 0.0
stringToFloat s = entera + fraccionaria
  where
    e = stringToInt $ takeWhile (/= '.') s
    entera = fromInteger $ toInteger e
    f = tail $ dropWhile (/= '.') s
    fracEnt = fromInteger $ toInteger $ stringToInt f
    fraccionaria = fracEnt / (10 ^ length f)

listasDePares :: [a] -> [[a]]
listasDePares [] = []
listasDePares lista = par : listasDePares resto
  where
    par = take 2 lista
    resto = drop 2 lista

distanciaEuclidea :: Float -> Float -> Float
distanciaEuclidea a b = sqrt $ (a - b) ** 2

intercambia :: [a] -> a -> Int -> [a]
intercambia (x : xs) a p
  | p == 0 = a : xs
  | null xs = [a]
  | otherwise = x : intercambia xs a (p -1)

introduce :: [a] -> a -> Int -> [a]
introduce (x : xs) a p
  | p == 0 = x : a : xs
  | null xs = x : [a]
  | otherwise = x : introduce xs a (p -1)

elimina :: [a] -> Int -> [a]
elimina (x : xs) p
  | p == 0 = xs
  | null xs = [x]
  | otherwise = x : elimina xs (p -1)

eliminaElemento :: Eq a => [a] -> a -> [a]
eliminaElemento (x : xs) a
  | x == a = xs
  | null xs = [x]
  | otherwise = x : eliminaElemento xs a

aleatorio :: Int -> [a] -> a
aleatorio al xs
  | null xs = error "Lista vacia en funcion aleatorio"
  | otherwise = x
  where
    limite = length xs
    a = mod al limite
    x = xs !! a

escogeAleatorios :: Double -> [a] -> IO [a]
escogeAleatorios _ [] = return []
escogeAleatorios al (x : xs) = do
  let prob = normaliza al
  tiempo <- time
  let contra = normaliza tiempo
  resto <- escogeAleatorios al xs
  if contra > prob
    then return $ x : resto
    else return resto

normaliza :: Double -> Double
normaliza d = d - fromIntegral (floor d)

now :: IO Int
now = getCurrentTime Data.Functor.<&> (floor . fromRational . toRational . utctDayTime)

time :: IO Double
time = getCurrentTime Data.Functor.<&> (fromRational . toRational . utctDayTime)

vacia :: Matrix Int
vacia = zero 1 1