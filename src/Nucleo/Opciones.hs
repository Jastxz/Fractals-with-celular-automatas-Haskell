module Nucleo.Opciones
  ( pintaOpciones,
    seleccionaOpciones,
    actualizaTextosOpciones,
    actualizaTextosEspecialOpciones,
  )
where

import GHC.Float (float2Int)
import Graphics.Gloss
import Tipos.TipoAlmacenElementos
import Tipos.TipoAutomata (automataVacio)
import Tipos.TipoElemento
import Tipos.TipoEntrada as En
import Tipos.TipoEtiqueta as Et
import Tipos.TipoForma as F
import Tipos.TipoMundo as M
import Tipos.TipoPosicion (alineamientoCentrado, creaMallaConPosicion, cuartaEsquina, moduloVector, moduloVectorEsquinas, primEsquina, secEsquina, tercEsquina)
import Tipos.TipoTitulo as T
import Utilidades.Constantes as C
import Utilidades.Ficheros (caminoArchivo, cargarAutomata, cargarFilaArchivo, existe, nombreArchivo)
import Utilidades.Utiles
import Utilidades.UtilesGraficos

pintaOpciones :: Mundo -> IO AlmacenElementos
pintaOpciones mundo
  | pantalla mundo == opciones = return $ almacen mundo
  | otherwise = do
      let mallaBase = creaMallaConPosicion tamañoVentana

      -- Redefinición de la malla que se va a usar para la intramalla
      let conjuntoEsquinasIntramalla@(intra1, intra2, intra3, intra4) = (mallaBase !! 21, mallaBase !! 28, mallaBase !! 71, mallaBase !! 78)
      let tamañoIntramalla@(anchoI, altoI) = (moduloVector (primEsquina intra1) (secEsquina intra2), moduloVector (primEsquina intra1) (tercEsquina intra3))
      let mallaInterior = creaMallaConPosicion (float2Int anchoI, float2Int altoI)

      -- Definición de los títulos de cada sección de opciones
      let esquinasTituloCondiciones = (primEsquina (mallaInterior !! 1), secEsquina (mallaInterior !! 5), tercEsquina (mallaInterior !! 1), cuartaEsquina (mallaInterior !! 5))
      let datosElementoTituloCondiciones = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasTituloCondiciones}
      let datosTituloCondiciones =
            metadatosTituloVacio
              { T.metadatosElemento = datosElementoTituloCondiciones,
                T.datosTamañoTitulo = 3,
                T.datosTexto = "Initial conditions"
              }

      let esquinasTituloReglas = (primEsquina (mallaInterior !! 31), secEsquina (mallaInterior !! 35), tercEsquina (mallaInterior !! 31), cuartaEsquina (mallaInterior !! 35))
      let datosElementoTituloReglas = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasTituloReglas}
      let datosTituloReglas =
            datosTituloCondiciones
              { T.metadatosElemento = datosElementoTituloReglas,
                T.datosTexto = "Enter rule (0-255)"
              }

      let esquinasTituloNumCels = (primEsquina (mallaInterior !! 51), secEsquina (mallaInterior !! 55), tercEsquina (mallaInterior !! 51), cuartaEsquina (mallaInterior !! 55))
      let datosElementoTituloNumCels = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasTituloNumCels}
      let datosTituloNumCels =
            datosTituloCondiciones
              { T.metadatosElemento = datosElementoTituloNumCels,
                T.datosTexto = "Enter the number of cells per row"
              }

      -- Definición de las etiquetas de las condiciones iniciales
      let esquinasEtiquetasCondicionRandom = mallaInterior !! 11
      let datosElementoEtiquetaCondRandom = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasEtiquetasCondicionRandom}
      let datosEtiquetaCondicionRandom =
            metadatosEtiquetaVacia
              { Et.metadatosElemento = datosElementoEtiquetaCondRandom,
                Et.datosTexto = "Random",
                Et.datosEscalasTexto = (0.15, 0.15),
                Et.datosAlineacion = alineamientoCentrado,
                Et.datosNombreElementoAsociado = etiquetaRandom
              }

      let esquinasEtiquetasCondicionOne = (primEsquina (mallaInterior !! 13), secEsquina (mallaInterior !! 15), tercEsquina (mallaInterior !! 13), cuartaEsquina (mallaInterior !! 15))
      let datosElementoEtiquetaCondOne = metaDatosElementoVacioFondoTransparente {datosEsquinas = esquinasEtiquetasCondicionOne}
      let datosEtiquetaCondicionOne =
            datosEtiquetaCondicionRandom
              { Et.metadatosElemento = datosElementoEtiquetaCondOne,
                Et.datosTexto = "One cell activated",
                Et.datosNombreElementoAsociado = etiquetaOneCell
              }

      -- Definición de los radio button de las condiciones iniciales
      let esquinasCheckRandomCondiciones = mallaInterior !! 11
      let datosElementoCheckRandom = metaDatosElementoVacioFondoBlanco {datosNombre = etiquetaRandom, datosEsquinas = esquinasCheckRandomCondiciones}
      let datosCheckRandom = metadatosFormaRadioButton {F.metadatosElemento = datosElementoCheckRandom, F.datosMarcar = condiciones mundo == etiquetaRandom}

      let esquinasCheckOneCondiciones = (primEsquina (mallaInterior !! 13), secEsquina (mallaInterior !! 15), tercEsquina (mallaInterior !! 13), cuartaEsquina (mallaInterior !! 15))
      let datosElementoCheckOne = metaDatosElementoVacioFondoBlanco {datosNombre = etiquetaOneCell, datosEsquinas = esquinasCheckOneCondiciones}
      let datosCheckOne = metadatosFormaRadioButton {F.metadatosElemento = datosElementoCheckOne, F.datosMarcar = condiciones mundo == etiquetaOneCell}

      -- Definición de las entradas del resto de opciones
      let esquinasEntradaReglas = mallaInterior !! 41
      let datosElementoEntradaReglas = metaDatosElementoVacioFondoBlanco {datosNombre = entradaRegla, datosEsquinas = esquinasEntradaReglas}
      let modulosEntradaReglas@(merAn, merAl) = moduloVectorEsquinas esquinasEntradaReglas
      let datosEntradaReglas =
            metadatosEntradaVacia
              { En.metadatosElemento = datosElementoEntradaReglas,
                En.datosTexto = show (regla mundo),
                En.datosAltura = merAl / 2,
                En.datosAnchura = merAn,
                En.datosPorcentajesTexto = (0.4, 0.58)
              }

      let esquinasEntradaNumCels = mallaInterior !! 61
      let datosElementoEntradaNumCels = metaDatosElementoVacioFondoBlanco {datosNombre = entradaNumCels, datosEsquinas = esquinasEntradaNumCels}
      let modulosEntradaNumCels@(mencAn, mencAl) = moduloVectorEsquinas esquinasEntradaNumCels
      let datosEntradaNumCels =
            datosEntradaReglas
              { En.metadatosElemento = datosElementoEntradaNumCels,
                En.datosTexto = show (celulas mundo),
                En.datosAltura = mencAl / 2,
                En.datosAnchura = mencAn
              }

      -- Definición de los botones
      let esquinasBotonPropiedades = (primEsquina (mallaInterior !! 81), secEsquina (mallaInterior !! 83), tercEsquina (mallaInterior !! 81), cuartaEsquina (mallaInterior !! 83))
      let datosElementoBotonPropiedades = metaDatosElementoVacioFondoBlanco {datosNombre = botonPropiedades, datosEsquinas = esquinasBotonPropiedades}
      let modulosBotonPropiedades@(mbpAn, mbpAl) = moduloVectorEsquinas esquinasBotonPropiedades
      let datosBotonPropiedades =
            metadatosFormaBoton
              { F.metadatosElemento = datosElementoBotonPropiedades,
                F.datosTexto = "Properties of selected rule",
                F.datosPorcentajesTexto = (0.05, 0.55),
                F.datosEscalasTexto = (0.2, 0.2),
                F.datosAltura = mbpAl,
                F.datosAnchura = mbpAn
              }

      let esquinasBotonPlay = (primEsquina (mallaInterior !! 86), secEsquina (mallaInterior !! 87), tercEsquina (mallaInterior !! 86), cuartaEsquina (mallaInterior !! 87))
      let datosElementoBotonPlay = metaDatosElementoVacioFondoBlanco {datosNombre = botonPlay, datosEsquinas = esquinasBotonPlay}
      let modulosBotonPlay@(mbplAn, mbplAl) = moduloVectorEsquinas esquinasBotonPlay
      let datosBotonPlay =
            datosBotonPropiedades
              { F.metadatosElemento = datosElementoBotonPlay,
                F.datosTexto = "Watch animation",
                F.datosPorcentajesTexto = (0.1, 0.55),
                F.datosAltura = mbplAl,
                F.datosAnchura = mbplAn
              }

      -- Definición y construcción del almacén
      let datosAlmacen =
            DatosAlmacen
              { datosBarras = [],
                datosBases = [],
                datosEntradas = [datosEntradaReglas, datosEntradaNumCels],
                datosEtiquetas = [datosEtiquetaCondicionRandom, datosEtiquetaCondicionOne],
                datosFormas = [datosCheckRandom, datosCheckOne, datosBotonPropiedades, datosBotonPlay],
                datosListas = [],
                datosParrafos = [],
                datosSaltosDeLinea = [],
                datosTitulos = [datosTituloCondiciones, datosTituloReglas, datosTituloNumCels]
              }
      return $ snd $ construyeAlmacen [] datosAlmacen

seleccionaOpciones :: Mundo -> String -> IO Mundo
seleccionaOpciones mundo nombreElPulsado
  | nombreElPulsado == etiquetaRandom = return mundo {condiciones = etiquetaRandom, automata = automataVacio}
  | nombreElPulsado == etiquetaOneCell = return mundo {condiciones = etiquetaOneCell, automata = automataVacio}
  | nombreElPulsado == botonPropiedades = return mundo {siguientePantalla = propiedades}
  | nombreElPulsado == botonPlay = do
      existeGuardado <- existe mundo
      let conds = condiciones mundo
      if existeGuardado && conds /= etiquetaRandom
        then do
          mundoAutomataCargado <- cargarAutomata mundo
          return mundoAutomataCargado {siguientePantalla = C.animacion}
        else return mundo {siguientePantalla = C.animacion}
  | otherwise = return mundo

actualizaTextosOpciones :: Mundo -> Elemento -> IO Mundo
actualizaTextosOpciones = actualizaTextosEspecialOpciones

actualizaTextosEspecialOpciones :: Mundo -> Elemento -> IO Mundo
actualizaTextosEspecialOpciones mundo elementoActual
  | nombreElemento == entradaRegla = return mundo {regla = reglaActualizada, automata = automataVacio}
  | nombreElemento == entradaNumCels = return mundo {celulas = celulasActualizadas, automata = automataVacio}
  | otherwise = return mundo
  where
    nombreElemento = nombre elementoActual
    entradasAlmacen = entradas $ almacen mundo
    reglaActualizada = stringToInt $ En.texto $ cabeza "actualizaTextosEspecialOpciones" entradasAlmacen
    celulasActualizadas = stringToInt $ En.texto $ entradasAlmacen !! 1
