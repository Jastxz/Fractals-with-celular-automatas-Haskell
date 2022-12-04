module Animacion(
  pintaAnimacion,
  esperaAnimacion,
  animaAutomata,
) where

import Graphics.Gloss
import Tipos
import Utiles
import UtilesGraficos
import Automata

tam :: Int
tam = 50

pintaAnimacion :: Mundo -> IO Picture
pintaAnimacion mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = undefined

esperaAnimacion :: Point -> Mundo -> IO Mundo
esperaAnimacion raton mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = undefined

animaAutomata :: Mundo -> IO Mundo
animaAutomata mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = undefined

aplicaRegla :: Int -> Automata -> Automata
aplicaRegla regla automata
  | regla==30 = undefined
  | regla==90 = undefined
  | regla==150 = undefined
  | otherwise = undefined

regla30 :: Int -> Int -> Int -> Int
regla30 izq central der
  | izq==1 && central==der==0 = 1
  | izq==0 && central==der==1 = 1
  | central==1 && izq==der==0 = 1
  | der==1 && izq==central==0 = 1
  | otherwise = 0

regla90 :: Int -> Int -> Int -> Int
regla90 izq central der
  | izq==1 && central==der==0 = 1
  | izq==der==0 && central==1 = 1
  | izq=central==0 && der==1 = 1
  | otherwise = 0

regla150 :: Int -> Int -> Int -> Int
regla150 izq central der
  | izq==central==der==1 = 1
  | izq==1 && central==der==0 = 1
  | izq==der==0 && central==1 = 1
  | izq=central==0 && der==1 = 1
  | otherwise = 0

modificaFilasMatriz :: Int -> Automata -> Automata




