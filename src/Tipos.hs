module Tipos (
    Automata,
    Pos,
    Mundo
) where

import Data.Matrix

type Automata = Matrix Int
type Pos = (Int, Int)

-- Mundo -> Pantalla, (Regla, Condiciones, Datos Automata), Animación, Adicional
type Mundo = (String, (Int, String, Automata), Bool, [[String]])