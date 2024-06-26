module Interaccion
  ( comenzarPrograma,
  )
where

import GHC.IO (unsafePerformIO)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Nucleo.Animacion
import Nucleo.Menu
import Nucleo.Opciones
import Nucleo.Propiedades
import Tipos.TipoAlmacenElementos (AlmacenElementos, dibujaAlmacen, distribuyeDistintasAccionesAutomaticamente, extraeRelaciones, gestionaActualizacionesPasivasElementos)
import Tipos.TipoAutomata
import Tipos.TipoMundo
import Tipos.TipoMundo as M
import Utilidades.Colores
import Utilidades.Constantes as C
import Utilidades.Entradas
import Utilidades.Error
import Utilidades.Ficheros

comenzarPrograma :: IO ()
comenzarPrograma = do
  -- Realizamos las preparaciones necesarias para ejecutar el programa
  preparaDirectorios
  -- Ejecución del programa con gráficos
  playIO ventana fondo tasaDeRefresco menuInicial dibujaMundo manejaEntrada actualiza

{- Componentes de la función principal -}
-- ---------------------------------------------------------------------------------
ventana :: Display
ventana = InWindow "Automata" tamañoVentana posicionVentana

fondo :: Color
fondo = gris

-- Cuantas veces se actualiza la función/el dibujo por segundo
tasaDeRefresco :: Int
tasaDeRefresco = 60

dibujaMundo :: Mundo -> IO Picture
dibujaMundo mundo = do
  almacen <- obtenAlmacenMundo mundo
  let base = extraeRelaciones almacen
  dibujaAlmacen almacen base

obtenAlmacenMundo :: Mundo -> IO AlmacenElementos
obtenAlmacenMundo mundo
  | not (null err) = pintaError mundo
  | pant == menu = pintaMenu
  | pant == opciones = pintaOpciones mundo
  | pant == propiedades = pintaPropiedades mundo
  | pant == C.animacion = pintaAnimacion mundo
  | otherwise = pintaError $ mundo {textoError = "Error en obtenAlmacenMundo"}
  where
    pant = pantalla mundo
    err = textoError mundo

manejaEntrada :: Event -> Mundo -> IO Mundo
manejaEntrada evento mundo = do
  let almacenActual = almacen mundo
  almacenActualizado <- distribuyeDistintasAccionesAutomaticamente evento almacenActual
  let mundoEventoIO 
        | EventKey (MouseButton LeftButton) Up _ raton <- evento = gestionaPulsacionPantallas raton $ mundo {almacen = almacenActualizado} 
        | EventKey (Char tecla) Up _ _ <- evento = gestionaTecleoPantallas almacenActual $ mundo {almacen = almacenActualizado} 
        | EventKey (SpecialKey tecla) Up _ _ <- evento = gestionaTecleoEspecialPantallas almacenActual $ mundo {almacen = almacenActualizado} 
        | otherwise = return mundo
  mundoEvento <- mundoEventoIO
  let mundoActualizado = actualizaMundo mundoEvento
  return mundoActualizado

actualiza :: Float -> Mundo -> IO Mundo
actualiza _ mundo = do
  let almacenActual = almacen mundo
  almacenActualizado <- gestionaActualizacionesPasivasElementos almacenActual
  let pant = pantalla mundo
  let mundoActualizado = mundo {almacen = almacenActualizado}
  if pant == C.animacion
    then animaAutomata mundoActualizado
    else return mundoActualizado

-- ---------------------------------------------------------------------------------
actualizaMundo :: Mundo -> Mundo
actualizaMundo mundo
  | pant == menu = menuInicial
  | pant == opciones = iniciaOpciones mundo
  | pant == propiedades = iniciaPropiedades mundo
  | pant == C.animacion = iniciaAnimacion mundo
  | otherwise = mundo
  where
    pant = siguientePantalla mundo
    err = textoError mundo

menuInicial :: Mundo
menuInicial =
  Mundo
    { pantalla = menu,
      siguientePantalla = "",
      textoError = "",
      regla = 30,
      condiciones = etiquetaOneCell,
      automata = automataVacio,
      automataGuardado = automataVacio,
      M.animacion = False,
      celulas = 100,
      fila = 0,
      almacen = unsafePerformIO pintaMenu
    }

iniciaOpciones :: Mundo -> Mundo
iniciaOpciones mundo = mundo {pantalla = opciones, siguientePantalla = "", M.animacion = False, almacen = unsafePerformIO $ pintaOpciones mundo}

iniciaPropiedades :: Mundo -> Mundo
iniciaPropiedades mundo = mundo {pantalla = propiedades, siguientePantalla = "", M.animacion = False, almacen = unsafePerformIO $ pintaPropiedades mundo}

iniciaAnimacion :: Mundo -> Mundo
iniciaAnimacion mundo = mundo {pantalla = C.animacion, siguientePantalla = "", M.animacion = True, almacen = unsafePerformIO $ pintaAnimacion mundo}