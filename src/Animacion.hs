module Animacion
  ( pintaAnimacion,
    esperaAnimacion,
    animaAutomata,
  )
where

import Automata
import Data.Matrix
import Graphics.Gloss
import Tipos
import Utiles
import UtilesGraficos

pintaAnimacion :: Mundo -> IO Picture
pintaAnimacion mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = do
  let numCels = nrows automata
  let ancho = 600.0
  let alto = 600.0
  let medAncho = ancho / 2
  let medAlto = alto / 2
  let anchoCelula = ancho / (fromIntegral numCels - 0.5)
  let altoCelula = alto / (fromIntegral numCels - 0.5)
  let tamCelula = rectangleSolid anchoCelula altoCelula
  let listaAnchuras = [(- medAncho), (- medAncho + anchoCelula) .. medAncho]
  let listaAlturas = [medAlto, (medAlto - altoCelula) .. (- medAlto)]
  let matrizPosiciones = fromList numCels numCels [(x, y) | y <- listaAlturas, x <- listaAnchuras]
  let posicionesMatriz = [(f, c) | f <- [1 .. numCels], c <- [1 .. numCels]]
  let celulas = map (\pos -> pintaCelula numCels tamCelula (matrizPosiciones ! pos) (automata ! pos)) posicionesMatriz
  let dibujoAutomata = pictures celulas
  volver <- creaBotonMedio posVolver "Back to options"
  pausar <- creaBoton posPausar "Pause"
  reanudar <- creaBoton posReanudar "Resume"
  let res = pictures [dibujoAutomata, volver, pausar, reanudar]
  return res

esperaAnimacion :: Point -> Mundo -> IO Mundo
esperaAnimacion raton mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = do
  let volver = pulsaCerca raton posVolver
  let pausar = pulsaCerca raton posPausar
  let reanudar = pulsaCerca raton posReanudar
  -- Cambiamos la información de la animación
  let mundoAejecutar
        | volver = iniciaOpciones
        | pausar = (pantalla, (regla, condiciones, automata), False, adicional)
        | reanudar = (pantalla, (regla, condiciones, automata), True, adicional)
        | otherwise = mundo
  return mundoAejecutar

animaAutomata :: Mundo -> IO Mundo
animaAutomata mundo@(pantalla, (regla, condiciones, automata), animacion, adicional)
  | not animacion = return mundo
  | nrows automata == 1 = do
    semilla <- now
    let cels = cabeza "animaAutomata" $ cabeza "animaAutomata" adicional
    let na
          | condiciones == "Random" = automataRandom semilla cels
          | otherwise = automataPreparado cels
    return (pantalla, (regla, condiciones, na), True, [[cels, "2"]])
  | otherwise = do
    let reg = numeroRegla regla
    let cels = cabeza "animaAutomata" $ cabeza "animaAutomata" adicional
    let fila = cabeza "animaAutomata" adicional !! 1
    let f
          | esInt fila = stringToInt fila
          | otherwise = error "Datos incorrectos en adicional en animaAutomata"
    let nuevo_automata = aplicaRegla f reg automata
    let fNueva = show (f+1)
    return (pantalla, (regla, condiciones, nuevo_automata), animacion, [[cels, fNueva]])

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

pintaCelula :: Int -> Picture -> Point -> Int -> Picture
pintaCelula numCels tamCelula (px, py) celula
  | celula == 1 = translate px py celulaViva
  | otherwise = translate px py celulaMuerta
  where
    celulaViva = color white tamCelula
    celulaMuerta = color black tamCelula

posVolver :: Point
posVolver = (-450.0, 300.0)

posPausar :: Point
posPausar = (-450.0, -300.0)

posReanudar :: Point
posReanudar = (450.0, -300.0)
