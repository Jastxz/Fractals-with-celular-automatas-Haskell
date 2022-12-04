module Animacion(
  pintaAnimacion,
  esperaAnimacion,
  animaAutomata,
) where

import Data.Matrix
import Graphics.Gloss
import Tipos
import Utiles
import UtilesGraficos
import Automata

pintaAnimacion :: Mundo -> IO Picture
pintaAnimacion mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = do
  let numCels = nrows automata
  let anchoCelula = 1400.0 / fromIntegral numCels
  let altoCelula = 700.0 / fromIntegral numCels
  let tamCelula = rectangleSolid anchoCelula altoCelula
  let listaAnchuras = [(-700.0), (-700.0 + anchoCelula) .. 700.0]
  let listaAlturas = [350.0, (350.0 - altoCelula) .. (-350.0)]
  let matrizPosiciones = fromList numCels numCels [(x,y) | x<-listaAnchuras, y<-listaAlturas]
  let posicionesMatriz = [(f,c) | f<-[1..numCels], c<-[1..numCels]]
  let celulas = map (\pos -> pintaCelula numCels tamCelula (matrizPosiciones ! pos) (automata ! pos)) posicionesMatriz
  let dibujoAutomata = pictures celulas
  let (vX,vY) = posVolver
  let volver = translate vX vY $ boton "Back to options" anchoBoton altoBoton
  let (pX,pY) = posPausar
  let pausar = translate pX pY $ boton "Pause" anchoBoton altoBoton
  let (rX,rY) = posReanudar
  let reanudar = translate rX rY $ boton "Resume" anchoBoton altoBoton
  let res = pictures [volver, pausar, reanudar, dibujoAutomata]
  return res

esperaAnimacion :: Point -> Mundo -> IO Mundo
esperaAnimacion raton mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = do
  let volver = pulsaCerca raton posVolver
  let pausar = pulsaCerca raton posPausar
  let reanudar = pulsaCerca raton posReanudar
  -- Cambiamos la información de la animación
  let mundoAejecutar
        | volver = ("opciones", (regla, condiciones, automata), False, adicional)
        | pausar = (pantalla, (regla, condiciones, automata), False, adicional)
        | reanudar = (pantalla, (regla, condiciones, automata), True, adicional)
        | otherwise = mundo
  return mundoAejecutar

animaAutomata :: Mundo -> IO Mundo
animaAutomata mundo@(pantalla, (regla, condiciones, automata), animacion, adicional)
  | not animacion = return mundo
  | nrows automata == 1 = inicializaAutomata mundo
  | otherwise = do
    let reg = numeroRegla regla
    let nuevo_automata = aplicaRegla reg automata
    return (pantalla, (regla, condiciones, nuevo_automata), animacion, adicional)

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

pintaCelula :: Int -> Picture -> Point -> Int -> Picture
pintaCelula numCels tamCelula pixeles celula
  | celula == 1 = celulaViva
  | otherwise = celulaMuerta
  where
    celulaViva = color white tamCelula
    celulaMuerta = color black tamCelula

posVolver :: Point
posVolver = (-450.0,330.0)

posPausar :: Point
posPausar = (-450.0,-330.0)

posReanudar :: Point
posReanudar = (450.0,-330.0)
