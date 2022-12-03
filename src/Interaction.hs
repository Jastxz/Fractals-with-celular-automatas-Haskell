module Interaction (
    mainInteraction
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tipos
import Utiles
import UtilesGraficos

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
dibujaMundo mundo = undefined

manejaEntrada :: Event -> Mundo -> IO Mundo
manejaEntrada evento mundo
  | EventKey (MouseButton LeftButton) Up _ raton <- evento = hazAccion raton mundo
  | otherwise = return mundo

-- Función de distribución de casos similar a dibujaMundo
hazAccion :: Point -> Mundo -> IO Mundo
hazAccion raton mundo = undefined

actualiza :: Float -> Mundo -> IO Mundo
actualiza f mundo = undefined

menuInicial :: Mundo
menuInicial = undefined
-- ---------------------------------------------------------------------------------