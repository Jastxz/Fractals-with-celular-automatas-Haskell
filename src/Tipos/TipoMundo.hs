module Tipos.TipoMundo (
    Mundo(..),
) where

import Tipos.TipoAutomata
import Tipos.TipoAlmacenElementos (AlmacenElementos, almacenVacio)
import Utilidades.Constantes as C

{- Mundo: 
    Pantalla: pantalla en la que se encuentra el programa
    SiguientePantalla: pantalla a la que tenemos que actualizar para continuar con el programa
    TextoError: texto del error, si es que ha sucedido alguno
    Regla: regla a ejecutar
    Condiciones: condiciones de inicio, random o normal
    Automata: datos del autómata, células vivas y muertas
    AutomataGuardado: datos del autómata si este estaba guardado
    Animación: si dejamos funcionar la animación
    Celulas: número de células por fila o columna
    Fila: fila actual en la que se encuentra la animación
    Almacen: elementos que conforman la pantalla. Parte obligatoria para usar la librería simplificada-}
data Mundo = Mundo {
    pantalla :: String,
    siguientePantalla :: String,
    textoError :: String,
    regla :: Int,
    condiciones :: String,
    automata :: Automata,
    automataGuardado :: Automata,
    animacion :: Bool,
    celulas :: Int,
    fila :: Int,
    almacen :: AlmacenElementos
} deriving (Show)