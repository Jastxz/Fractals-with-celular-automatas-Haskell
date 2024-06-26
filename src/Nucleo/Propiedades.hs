module Nucleo.Propiedades
  ( pintaPropiedades,
    esperaPropiedades,
  )
where

import Graphics.Gloss
import Nucleo.Automata
import Tipos.TipoAlmacenElementos
import Tipos.TipoElemento
import Tipos.TipoForma as F
import Tipos.TipoMundo
import Tipos.TipoPosicion
import Utilidades.Constantes as C
import Utilidades.Utiles
import Utilidades.UtilesGraficos
import Tipos.TipoLista as L

pintaPropiedades :: Mundo -> IO AlmacenElementos
pintaPropiedades mundo = do
  let mallaBase = creaMallaConPosicion tamañoVentana

  -- Definición de la lista
  let esquinasLista = (primEsquina (mallaBase !! 33), secEsquina (mallaBase !! 36), tercEsquina (mallaBase !! 53), cuartaEsquina (mallaBase !! 56))
  let datosElementoLista = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasLista}
  let textoRegla = show $ regla mundo
  let texto = propiedadesRegla $ regla mundo
  let datosLista = metadatosListaVacia {
      L.metadatosElemento = datosElementoLista,
      datosEncabezado = "Properties of rule " ++ textoRegla,
      datosEscalaEncabezado = (0.3, 0.3),
      datosTextos = texto,
      L.datosEscalasTexto = (0.15, 0.15)
  }

  -- Definición de los botones
  let esquinasBotonBack = (primEsquina (mallaBase !! 71), secEsquina (mallaBase !! 72), tercEsquina (mallaBase !! 71), cuartaEsquina (mallaBase !! 72))
  let datosElementoBotonBack = metaDatosElementoVacioFondoBlanco {datosNombre = botonBack, datosEsquinas = esquinasBotonBack}
  let modulosBotonBack@(mbbAn, mbbAl) = moduloVectorEsquinas esquinasBotonBack
  let datosBotonBack =
        F.metadatosFormaBoton
          { F.metadatosElemento = datosElementoBotonBack,
            F.datosTexto = "Back to options",
            F.datosPorcentajesTexto = (0.05, 0.55),
            F.datosEscalasTexto = (0.2, 0.2),
            F.datosAltura = mbbAl,
            F.datosAnchura = mbbAn
          }

  let esquinasBotonPlay = (primEsquina (mallaBase !! 77), secEsquina (mallaBase !! 78), tercEsquina (mallaBase !! 77), cuartaEsquina (mallaBase !! 78))
  let datosElementoBotonPlay = metaDatosElementoVacioFondoBlanco {datosNombre = botonPlay, datosEsquinas = esquinasBotonPlay}
  let modulosBotonResume@(mbrAn, mbrAl) = moduloVectorEsquinas esquinasBotonPlay
  let datosBotonPlay =
        F.metadatosFormaBoton
          { F.metadatosElemento = datosElementoBotonPlay,
            F.datosTexto = "Watch animation",
            F.datosPorcentajesTexto = (0.1, 0.55),
            F.datosEscalasTexto = (0.2, 0.2),
            F.datosAltura = mbrAl,
            F.datosAnchura = mbrAn
          }

  -- Definición y construcción del almacén
  let datosAlmacen =
        DatosAlmacen
          { datosBarras = [],
            datosBases = [],
            datosEntradas = [],
            datosEtiquetas = [],
            datosFormas = [datosBotonBack, datosBotonPlay],
            datosListas = [datosLista],
            datosParrafos = [],
            datosSaltosDeLinea = [],
            datosTitulos = []
          }
  return $ snd $ construyeAlmacen [] datosAlmacen

esperaPropiedades :: Mundo -> String -> IO Mundo
esperaPropiedades mundo nombreElPulsado
  | nombreElPulsado == botonBack = return $ mundo {siguientePantalla = opciones}
  | nombreElPulsado == botonPlay = return $ mundo {siguientePantalla = C.animacion}
  | otherwise = return mundo
