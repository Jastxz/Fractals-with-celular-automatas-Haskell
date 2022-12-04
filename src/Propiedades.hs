module Propiedades(
  pintaPropiedades,
  esperaPropiedades,
) where

import Graphics.Gloss
import Tipos
import Utiles
import UtilesGraficos
import Automata

pintaPropiedades :: Mundo -> IO Picture
pintaPropiedades mundo@(pantalla, (regla, condicion, automata), animacion, adicional) = do
  -- Receptáculo para mostrar las propiedades
  let borde = rectangleWire 1000 500
  -- Título
  let textoTitulo = "Chaotic properties of rule " ++ traduceRegla regla
  let titulo = translate (-450.0) (-250.0) $ texto textoTitulo
  -- Dibujando las condiciones iniciales
  let comienzoLista = 220.0
  let evolucionLista = -40.0
  let textosPropiedades = propiedadesRegla regla
  let propiedades = pictures $ listaTextos textosPropiedades 'Y' comienzoLista evolucionLista True
  -- Preparamos los botones y la lista para crear la imagen
  let (vX, vY) = posVolver
  let prop = translate vX vY $ boton "Back to options" anchoBoton altoBoton
  let (aX, aY) = posAnim
  let anim = translate aX aY $ boton "Watch animation" anchoBoton altoBoton
  let listaRes = [titulo, propiedades, prop, anim]
  -- Resultado
  let res = pictures listaRes
  return res

esperaPropiedades :: Point -> Mundo -> IO Mundo
esperaPropiedades raton mundo@(pantalla, (regla, condicion, automata), animacion, adicional) = do
  -- Miramos qué botón ha sido pulsado
  let volver = pulsaCerca raton posVolver
  let animacion = pulsaCerca raton posAnim
  -- Cambiamos el mundo según si se ha pulsado algún botón
  let mundoAejecutar
        | volver = ("propiedades", (regla, condicion, automata), animacion, adicional)
        | animacion = ("animacion", (regla, condicion, automata), True, adicional)
        | otherwise = mundo
  return mundoAejecutar

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

posVolver :: Point
posVolver = (-450.0,-300.0)

posAnim :: Point
posAnim = (100.0,-300.0)

traduceRegla :: Int -> String
traduceRegla regla
  | regla == 0 = "30"
  | regla == 1 = "90"
  | regla == 2 = "150"
  | otherwise = error $ "En traduceRegla no esta entrando una regla valida. Entrada: " ++ show regla
