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
  let textoTitulo = "Properties of rule " ++ traduceRegla regla
  let titulo = translate (-250.0) 250.0 $ texto textoTitulo
  -- Dibujando las condiciones iniciales
  let comienzoLista = 200.0
  let evolucionLista = -40.0
  let textosPropiedades = propiedadesRegla regla
  let propiedades = translate 200.0 0.0 $ pictures $ listaTextos textosPropiedades 'Y' comienzoLista evolucionLista True
  -- Preparamos los botones y la lista para crear la imagen
  volver <- creaBotonMedio posVolver "Back to options"
  anim <- creaBotonMedio posAnim "Watch animation"
  let listaRes = [titulo, propiedades, volver, anim]
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
        | volver = ("opciones", (regla, condicion, automata), animacion, adicional)
        | animacion = ("animacion", (regla, condicion, automata), True, adicional)
        | otherwise = mundo
  return mundoAejecutar

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

posVolver :: Point
posVolver = (-250.0,-150.0)

posAnim :: Point
posAnim = (200.0,-150.0)

traduceRegla :: Int -> String
traduceRegla regla
  | regla == 0 = "30"
  | regla == 1 = "73"
  | regla == 2 = "90"
  | regla == 3 = "105"
  | regla == 4 = "122"
  | regla == 5 = "124"
  | regla == 6 = "126"
  | regla == 7 = "150"
  | regla == 8 = "193"
  | regla == 9 = "195"
  | otherwise = error $ "En traduceRegla no esta entrando una regla valida. Entrada: " ++ show regla
