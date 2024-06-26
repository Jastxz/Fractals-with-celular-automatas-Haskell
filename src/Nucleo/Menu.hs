module Nucleo.Menu
  ( pintaMenu,
    esperaComienzo,
  )
where

import qualified Data.Bifunctor
import GHC.Float (int2Float)
import GHC.IO (unsafePerformIO)
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Tipos.TipoAlmacenElementos
import Tipos.TipoAutomata (automataVacio)
import Tipos.TipoElemento (MetaDatos (datosEsquinas, datosNombre), metaDatosElementoVacioFondoBlanco, metaDatosElementoVacioFondoTransparente)
import Tipos.TipoForma as F
import Tipos.TipoMundo
import Tipos.TipoParrafo as P
import Tipos.TipoPosicion (creaMallaConPosicion, parteEsquinas)
import Utilidades.Constantes
import Utilidades.Utiles
import Utilidades.UtilesGraficos

pintaMenu :: IO AlmacenElementos
pintaMenu = do
  let (anchoV, altoV) = Data.Bifunctor.bimap int2Float int2Float tamañoVentana
  let mallaBase = creaMallaConPosicion tamañoVentana

  -- Definición datos del párrafo de bienvenida
  let posBienvenida = (0.0, 0.0 + altoV / 10)
  let conjuntoEsquinasBienvenida@(a, b, c, d) = (mallaBase !! 33, mallaBase !! 36, mallaBase !! 43, mallaBase !! 46)
  let esquinasBienvenida = (parteEsquinas a 1, parteEsquinas b 2, parteEsquinas c 3, parteEsquinas d 4)
  let datosElementoParrafoBienvenida = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasBienvenida}
  let textoBienvenida = "Welcome! With this application you will generate fractal patterns using cellular automata. To get started, click on the button below."
  let datosParrafoMenu =
        metadatosParrafoVacio
          { P.metadatosElemento = datosElementoParrafoBienvenida,
            P.datosTexto = textoBienvenida,
            P.datosEscalasTexto = (0.15, 0.15),
            P.datosMargenes = (0.0, 0.0)
          }

  -- Definición datos del botón
  let posBoton = (0.0, 0.0 - altoV / 10)
  let conjuntoEsquinasBoton@(e, f) = (mallaBase !! 64, mallaBase !! 65)
  let esquinasBoton = (parteEsquinas e 1, parteEsquinas f 2, parteEsquinas e 3, parteEsquinas f 4)
  let datosElementoBotonMenu = metaDatosElementoVacioFondoBlanco {datosNombre = botonIniciaOpciones, datosEsquinas = esquinasBoton}
  let datosBotonMenu =
        metadatosFormaBoton
          { F.metadatosElemento = datosElementoBotonMenu,
            F.datosTexto = "Start",
            F.datosPorcentajesTexto = (0.43, 0.525),
            F.datosEscalasTexto = (0.15, 0.15),
            F.datosAltura = altoV / 10,
            F.datosAnchura = anchoV / 10
          }

  -- Definición y construcción del almacén
  let datosAlmacen =
        DatosAlmacen
          { datosBarras = [],
            datosBases = [],
            datosEntradas = [],
            datosEtiquetas = [],
            datosFormas = [datosBotonMenu],
            datosListas = [],
            datosParrafos = [datosParrafoMenu],
            datosSaltosDeLinea = [],
            datosTitulos = []
          }
  return $ snd $ construyeAlmacen [] datosAlmacen

esperaComienzo :: Mundo -> String -> IO Mundo
esperaComienzo mundo nombreElPulsado
  | nombreElPulsado == botonIniciaOpciones = return $ mundo {siguientePantalla = opciones}
  | otherwise = return mundo