module Interaction (
    mainInteraction
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos
import Opciones
import Propiedades
import Animacion

mainInteraction :: IO ()
mainInteraction = do
  -- Ejecución del programa con gráficos
  playIO ventanaJuego fondo tasaDeRefresco menuInicial dibujaMundo manejaEntrada actualiza

{- Componentes de la función principal -}
-- ---------------------------------------------------------------------------------
ventanaJuego :: Display
ventanaJuego = InWindow "Automata" (1400, 700) (80, 70)

fondo :: Color
fondo = gris

-- Cuantas veces se actualiza el dibujo cada segundo
tasaDeRefresco :: Int
tasaDeRefresco = 5

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
  | pantalla == "menu" = esperaComienzo raton mundo
  | pantalla == "opciones" = seleccionaOpciones raton mundo
  | pantalla == "propiedades" = esperaPropiedades raton mundo
  | pantalla == "animacion" = esperaAnimacion raton mundo
  | otherwise = error "Pantalla desconocida en hazAccion"

actualiza :: Float -> Mundo -> IO Mundo
actualiza _ mundo@(pantalla, (regla, condiciones, automata), animacion, adicional)
  | pantalla == "animacion" = animaAutomata mundo
  | otherwise = return mundo
-- ---------------------------------------------------------------------------------

{- Funciones de dibujo y gestión de E/S del menú inicial -}
-- ---------------------------------------------------------------------------------
pintaMenu :: IO Picture
pintaMenu = do
  let inicioCasillas = -420.0
  let parte1 = translate inicioCasillas (head alturasCasillas) $ texto "Welcome! With this application you will generate"
  let parte2 = translate inicioCasillas (alturasCasillas !! 1) $ texto "fractal patterns using cellular automata."
  let parte3 = translate inicioCasillas (alturasCasillas !! 2) $ texto "To get started, click on the button below."
  btn <- creaBoton posBoton "Start"
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
posBoton = (0.0,-210.0)

ancho :: Float
ancho = 500.0 / 2.0

ajusteInicialMenu :: Float
ajusteInicialMenu = ancho / (2 * 8.0)

alturasCasillas :: [Float]
alturasCasillas = [a, a - diferencia .. -150.0]
  where
    a = ancho - ajusteInicialMenu * 2
    diferencia = a / 4.0