module Opciones
  ( pintaOpciones,
    seleccionaOpciones,
  )
where

import Graphics.Gloss
import Tipos
import Utiles
import UtilesGraficos

pintaOpciones :: Mundo -> IO Picture
pintaOpciones mundo@(pantalla, (regla, condicion, automata), animacion, adicional) = do
  -- Valores de separación entre las casillas de las opciones
  let inicioCasillas = fst distribucionOpciones
  let evolucionCasillas = snd distribucionOpciones
  -- Receptáculo para mostrar las opciones
  let borde = rectangleWire 1000 500
  -- Dibujando las condiciones iniciales
  let tituloCon = translate inicioCasillas (head alturasCasillas) $ texto "Initial conditions"
  let condiciones = head infoEstatica
  let dibCondiciones = translate 0 (alturasCasillas !! 1) $ pictures $ listaTextos condiciones 'X' inicioCasillas evolucionCasillas False
  let lCondiciones = length condiciones
  let cond | condicion == "unaActiva" = 1
        | otherwise = 0
  let cbx1 = pictures $ dibujaCheckbox (lCondiciones - 1) cond 'X' inicioCasillas evolucionCasillas
  let checkboxCondiciones = translate 0 (alturasCasillas !! 2) cbx1
  -- Dibujando las reglas disponibles
  let tituloReglas = translate inicioCasillas (alturasCasillas !! 3) $ texto "Choose rule"
  let reglas = infoEstatica !! 1
  let dibReglas = translate 0 (alturasCasillas !! 4) $ pictures $ listaTextos reglas 'X' inicioCasillas evolucionCasillas False
  let lReglas = length reglas
  let cbx2 = pictures $ dibujaCheckbox (lReglas - 1) regla 'X' inicioCasillas evolucionCasillas
  let checkboxReglas = translate 0 (alturasCasillas !! 5) cbx2
  -- Preparamos los botones y la lista para crear la imagen
  let (pX, pY) = posProp
  let prop = translate pX pY $ boton "Chaotic properties of selected rule" anchoBotonExtraLargo altoBotonExtraLargo
  let (aX, aY) = posAnim
  let anim = translate aX aY $ boton "Watch animation" anchoBotonMedio altoBotonMedio
  let listaRes1 = [borde, tituloCon, dibCondiciones, checkboxCondiciones]
  let listaRes2 = [tituloReglas, dibReglas, checkboxReglas, prop, anim]
  let listaRes = listaRes1 ++ listaRes2
  -- Resultado
  let res = pictures listaRes
  return res

seleccionaOpciones :: Point -> Mundo -> IO Mundo
seleccionaOpciones raton@(x,y) mundo = do
  -- Valores de separación entre las casillas de las opciones
  let iC = fst distribucionOpciones
  let eC = snd distribucionOpciones
  -- Buscando la casilla en cuestión
  let indice = minimum [if cercaBox y altura then p else 99 | (altura, p) <- zip alturasEstaticas [0 ..]]
  let fila
        | indice == 99 = head infoEstatica
        | otherwise = infoEstatica !! indice
  let limite = length fila
  let indice2 = minimum [if cercaBox x longitud then p else 99 | (longitud, p) <- zip [iC, iC + eC ..] [0 .. (limite - 1)]]
  let columna
        | indice == 99 || indice2 == 99 = head fila
        | otherwise = fila !! indice2
  let propiedades = pulsaCerca raton posProp
  let animacion = pulsaCerca raton posAnim
  -- Cambiamos la información de la animación
  nuevoMundo@(prop, (reg, cond, aut), an, ad) <- cambiaOpcion raton mundo indice columna
  let mundoAejecutar
        | propiedades = ("propiedades", (reg, cond, aut), an, ad)
        | animacion = ("animacion", (reg, cond, aut), True, ad)
        | otherwise = nuevoMundo
  return mundoAejecutar

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

posProp :: Point
posProp = (-240.0,-180.0)

posAnim :: Point
posAnim = (200.0,-180.0)

distribucionOpciones :: Point
distribucionOpciones = (-450.0, 130.0)

ancho :: Float
ancho = 500.0 / 2.0

ajusteInicialMenu :: Float
ajusteInicialMenu = ancho / (2 * 8.0)

infoEstatica :: [[String]]
infoEstatica = [condiciones, reglas]
  where
    condiciones = ["Random", "One cell activated"]
    reglas = ["30", "90", "150"]

alturasEstaticas :: [Float]
alturasEstaticas = [condiciones, reglas]
  where
    condiciones = alturasCasillas !! 2
    reglas = alturasCasillas !! 5

alturasCasillas :: [Float]
alturasCasillas = [a, a - diferencia .. -150.0]
  where
    a = ancho - ajusteInicialMenu * 2
    diferencia = a / 5.0

cambiaOpcion :: Point -> Mundo -> Int -> String -> IO Mundo
cambiaOpcion raton mundo@(pantalla, (regla, condicion, automata), animacion, adicional) nivel opcion
  | nivel == 0 = do
    let nuevoMundo = (pantalla, (regla, opcion, automata), animacion, adicional)
    return nuevoMundo
  | nivel == 1 = do
    let nuevoMundo = (pantalla, (traduceRegla opcion, condicion, automata), animacion, adicional)
    return nuevoMundo
  | nivel == 99 = return mundo
  | otherwise = error "El nivel de opciones especificado para la función cambiaOpción."

traduceRegla :: String -> Int
traduceRegla regla
  | regla == "aleatoria" = 0
  | regla == "unaActiva" = 1
  | otherwise = error $ "En traduceRegla no esta entrando una regla valida. Entrada: " ++ regla
