module Tests.PruebasEntrada
  ( testAlmacen,
  )
where

import Graphics.Gloss.Interface.Pure.Display (Display (InWindow), blue, display, polygon, white, Picture)
import Tests.DatosElementosPreconstruidos as Dep
import Tipos.TipoAlmacenElementos as Al
import Tipos.TipoBarraNavegacion as Barra
import qualified Tipos.TipoBarraNavegacion as Barra
import Tipos.TipoBase as B
import Tipos.TipoElemento as E
import Tipos.TipoEntrada as E
import Tipos.TipoEtiqueta as Et
import Tipos.TipoForma as F
import Tipos.TipoLista as L
import Tipos.TipoParrafo as P
import Tipos.TipoPosicion (Malla, esquinasAlista)
import Tipos.TipoSaltoDeLinea as S
import Tipos.TipoTitulo as T
import qualified Tipos.TipoTitulo as T
import Utilidades.Constantes (posicionVentana, tamañoVentana)
import Graphics.Gloss.Interface.IO.Game (playIO, Event (EventKey), Key (MouseButton, Char, SpecialKey), MouseButton (LeftButton), KeyState (Up))
import GHC.IO (unsafePerformIO)

-- Definiciones básicas

ventana :: Display
ventana = InWindow "Automata" tamañoVentana posicionVentana

almacenConstruido :: ([String], Al.AlmacenElementos)
almacenConstruido = Al.construyeAlmacen [] almacenInput

almacen :: Al.AlmacenElementos
almacen = snd almacenConstruido

base :: B.Base
base = extraeRelaciones almacen

-- Definiciones para las entradas

manejaEntrada :: Event -> AlmacenElementos -> IO AlmacenElementos
manejaEntrada evento mundo
  | EventKey (MouseButton LeftButton) Up _ raton <- evento = gestionaPulsacionElemento mundo raton
  | EventKey (Char tecla) Up _ _ <- evento = gestionaTeclaChar tecla mundo
  | EventKey (SpecialKey tecla) Up _ _ <- evento = gestionaTeclaEspecial tecla mundo
  | otherwise = return mundo

actualiza :: Float -> AlmacenElementos -> IO AlmacenElementos
actualiza _ mundo = gestionaActualizacionesPasivasElementos mundo

-- Tests

dibujaAlmacenAux :: Al.AlmacenElementos -> IO Picture
dibujaAlmacenAux almacenEl = dibujaAlmacen almacenEl base 

testAlmacen :: IO ()
testAlmacen = do
  playIO ventana white 15 almacen dibujaAlmacenAux manejaEntrada actualiza