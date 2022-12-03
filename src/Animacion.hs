module Animacion(
  pintaAnimacion,
  esperaAnimacion,
) where

import Utiles
import UtilesGraficos

pintaAnimacion :: Mundo -> IO Picture
pintaAnimacion mundo@(pantalla, (regla, condiciones, automata), animacion, adicional) = undefined