module Utilidades.Error
  ( pintaError,
  )
where

import Graphics.Gloss
import Tipos.TipoAlmacenElementos
import Tipos.TipoElemento as E
import Tipos.TipoMundo
import Tipos.TipoParrafo as P
import Tipos.TipoPosicion
import Tipos.TipoTitulo as T
import Utilidades.Constantes as C

pintaError :: Mundo -> IO AlmacenElementos
pintaError mundo = do
  let mallaBase = creaMallaConPosicion tamañoVentana

  -- Definición de los títulos
  let esquinasTitulo = mallaBase !! 3
  let datosElementoTitulo = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasTitulo}
  let datosTitulo = metadatosTituloVacio {T.metadatosElemento = datosElementoTitulo, T.datosTexto = "Has happened an error", T.datosTamañoTitulo = 3, T.datosAlineacion = alineamientoCentrado}

  let esquinasError = mallaBase !! 23
  let datosElementoError = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasError}
  let datosError = metadatosTituloVacio {T.metadatosElemento = datosElementoError, T.datosTexto = textoError mundo, T.datosTamañoTitulo = 3, T.datosAlineacion = alineamientoCentrado}

  let esquinasEscape = mallaBase !! 72
  let datosElementoEscape = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasEscape}
  let datosEscape = metadatosTituloVacio {T.metadatosElemento = datosElementoEscape, T.datosTexto = "Must restart the program if wanna use it.Sorry for the problems", T.datosTamañoTitulo = 3}

  -- Definición del párrafo
  let esquinasParrafo = (primEsquina (mallaBase !! 43), secEsquina (mallaBase !! 46), tercEsquina (mallaBase !! 53), cuartaEsquina (mallaBase !! 56))
  let datosElementoParrafo = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasParrafo}
  let textoExplicativo1 = "If you wish, you can take a screenshot of the error described and send it to my email: javicraft14@gmail.com. "
  let textoExplicativo2 = "Add a small description of what were you doing when the error raised."
  let gracias = "Thank you for your help."
  let texto = textoExplicativo1 ++ textoExplicativo2 ++ gracias
  let datosParrafo =
        metadatosParrafoVacio
          { P.metadatosElemento = datosElementoParrafo,
            P.datosTexto = texto,
            P.datosEscalasTexto = (0.15, 0.15),
            P.datosMargenes = (0.0, 0.0)
          }

  -- Definición y construcción del almacén
  let datosAlmacen =
        DatosAlmacen
          { datosBarras = [],
            datosBases = [],
            datosEntradas = [],
            datosEtiquetas = [],
            datosFormas = [],
            datosListas = [],
            datosParrafos = [datosParrafo],
            datosSaltosDeLinea = [],
            datosTitulos = [datosTitulo, datosError, datosEscape]
          }
  return $ snd $ construyeAlmacen [] datosAlmacen