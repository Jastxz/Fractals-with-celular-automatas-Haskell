module Interaction (
    mainInteraction
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos
import Opciones
import Animacion

mainInteraction :: IO ()
mainInteraction = do
  -- Ejecución del programa con gráficos
  playIO ventanaJuego fondo tasaDeRefresco menuInicial dibujaMundo manejaEntrada actualiza

{- Componentes de la función principal -}
-- ---------------------------------------------------------------------------------
ventanaJuego :: Display
ventanaJuego = InWindow "Automata" (1400, 700) (50, 40)

fondo :: Color
fondo = gris

-- Cuantas veces se actualiza el dibujo cada segundo
tasaDeRefresco :: Int
tasaDeRefresco = 1

dibujaMundo :: Mundo -> IO Picture
dibujaMundo mundo@(pantalla, (regla, condiciones, automata), animacion, adicional)
  | pantalla == "menu" = pintaMenu
  | pantalla == "opciones" = pintaOpciones mundo
  | pantalla == "propiedades" = pintaPropiedades mundo
  | pantalla == "animacion" = pintaAnimacion mundo
  | otherwise = error "Pantalla desconocida en dibujaMundo"

manejaEntrada :: Event -> Mundo -> IO Mundo
manejaEntrada evento mundo
  | EventKey (MouseButton LeftButton) Up _ raton <- evento = hazAccion raton mundo
  | otherwise = return mundo

hazAccion :: Point -> Mundo -> IO Mundo
hazAccion raton mundo@(pantalla, (regla, condiciones, automata), animacion, adicional)
  | pantalla == "menu" = esperaComienzo raton
  | pantalla == "opciones" = seleccionaOpciones raton mundo
  | pantalla == "propiedades" = esperaPropiedades raton mundo
  | pantalla == "animacion" = esperaAnimacion raton mundo
  | otherwise = error "Pantalla desconocida en hazAccion"

actualiza :: Float -> Mundo -> IO Mundo
actualiza _ mundo
  | pantalla == "animacion" = animaAutomata mundo
  | otherwise = return mundo
-- ---------------------------------------------------------------------------------

{- Funciones de dibujo y gestión de E/S del menú inicial -}
-- ---------------------------------------------------------------------------------
pintaMenu :: IO Picture
pintaMenu = do
  let titulos = pictures $ listaTextos listaDeJuegos 'Y' comienzoLista evolucionLista True
  let parte1 = "Primera parte del mensaje de bienvenida"
  let parte2 = "segunda parte del mensaje de bienvenida"
  let parte3 = "recuerda que todo debe estar en ingles"
  let (bX,bY) = posBoton
  let btn = translate bX bY $ boton "Start" anchoBoton altoBoton
  let res = pictures [parte1,parte2,parte3,btn]
  return res

esperaComienzo :: Point -> Mundo -> IO Mundo
esperaComienzo raton mundo = do
  let cerca = pulsaCerca raton posBoton
  let nuevoMundo | cerca = iniciaOpciones | otherwise = mundo
  return nuevoMundo
-- ---------------------------------------------------------------------------------

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
posBoton :: Point
posBoton = (0.0,-340.0)