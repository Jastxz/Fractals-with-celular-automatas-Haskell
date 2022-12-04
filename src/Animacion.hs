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

pintaAnimacion :: Mundo -> IO Picture
pintaAnimacion mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = undefined

esperaAnimacion :: Point -> Mundo -> IO Mundo
esperaAnimacion raton mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = undefined

animaAutomata :: Mundo -> IO Mundo
animaAutomata mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = undefined




