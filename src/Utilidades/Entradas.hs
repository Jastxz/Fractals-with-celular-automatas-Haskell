module Utilidades.Entradas
  ( gestionaPulsacionPantallas,
    gestionaTecleoPantallas,
    gestionaTecleoEspecialPantallas,
  )
where

import Graphics.Gloss.Interface.IO.Game
import Nucleo.Animacion
import Nucleo.Menu
import Nucleo.Opciones
import Nucleo.Propiedades
import Tipos.TipoAlmacenElementos (gestionaPulsacionElemento, gestionaTeclaChar, gestionaTeclaEspecial, nombreElementoPulsado, elementoReferido, devuelveElementoPulsado, AlmacenElementos)
import Tipos.TipoMundo
import Tipos.TipoPosicion (PosF)
import Utilidades.Constantes as Constantes
import Utilidades.Error
import GHC.IO (unsafePerformIO)

gestionaPulsacionPantallas :: PosF -> Mundo -> IO Mundo
gestionaPulsacionPantallas raton mundo
  | pant == menu = esperaComienzo mundo nombreElPulsado
  | pant == opciones = seleccionaOpciones mundo nombreElPulsado
  | pant == propiedades = esperaPropiedades mundo nombreElPulsado
  | pant == Constantes.animacion = esperaAnimacion mundo nombreElPulsado
  | otherwise = return mundo {textoError = "Error en gestionaPulsacionPantallas"}
  where
    pant = pantalla mundo
    almacenActual = almacen mundo
    nombreElPulsado = nombreElementoPulsado almacenActual raton

gestionaTecleoPantallas :: AlmacenElementos -> Mundo -> IO Mundo
gestionaTecleoPantallas almacenAnterior mundo
  | pant == menu = return mundo
  | pant == opciones = actualizaTextosOpciones mundo elementoActual
  | pant == propiedades = return mundo
  | pant == Constantes.animacion = return mundo
  | otherwise = return mundo {textoError = "Error en gestionaTecleoPantallas"}
  where
    pant = pantalla mundo
    elementoActual = devuelveElementoPulsado almacenAnterior

gestionaTecleoEspecialPantallas :: AlmacenElementos -> Mundo -> IO Mundo
gestionaTecleoEspecialPantallas almacenAnterior mundo
  | pant == menu = return mundo
  | pant == opciones = actualizaTextosEspecialOpciones mundo elementoActual
  | pant == propiedades = return mundo
  | pant == Constantes.animacion = return mundo
  | otherwise = return mundo {textoError = "Error en gestionaTecleoEspecialPantallas"}
  where
    pant = pantalla mundo
    elementoActual = devuelveElementoPulsado almacenAnterior