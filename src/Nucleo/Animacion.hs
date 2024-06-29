module Nucleo.Animacion
  ( pintaAnimacion,
    esperaAnimacion,
    animaAutomata,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Matrix
import GHC.Float
import Graphics.Gloss
import Nucleo.Automata
import Tipos.TipoAlmacenElementos
import Tipos.TipoAutomata
import Tipos.TipoElemento
import qualified Tipos.TipoForma as F
import Tipos.TipoMundo as M
import Tipos.TipoPosicion
import Utilidades.Constantes as C
import Utilidades.Ficheros
import Utilidades.Utiles
import Utilidades.UtilesGraficos

pintaAnimacion :: Mundo -> IO AlmacenElementos
pintaAnimacion mundo = do
  let mallaBase = creaMallaConPosicion tamañoVentana

  -- Redefinición de la malla que se va a usar para la animación
  let conjuntoEsquinasIntramalla@(intra1, intra2, intra3, intra4) = (mallaBase !! 23, mallaBase !! 26, mallaBase !! 73, mallaBase !! 76)
  let tamañoIntramalla@(anchoI, altoI) = (moduloVector (primEsquina intra1) (secEsquina intra2), moduloVector (primEsquina intra1) (tercEsquina intra3))
  let mallaInterior = creaMallaConPosicion (float2Int anchoI, float2Int altoI)

  -- Dibujos de las células
  let aut = automata mundo
  let numCels = length aut
  let esquinasFondoAnimacion = (primEsquina (cabeza "pintaAnimacion" mallaInterior), secEsquina (mallaInterior !! 9), tercEsquina (mallaInterior !! 90), cuartaEsquina (mallaInterior !! 99))
  let modulosFondoAnimacion@(mfAn, mfAl) = moduloVectorEsquinas esquinasFondoAnimacion
  let tamañoCelula@(anC, alC) = (float2Int mfAn `div` numCels, float2Int mfAl `div` numCels)
  let posicionPrimeraCelula@(px, py) =
        Data.Bifunctor.bimap
          (int2Float (anC `div` 2) +)
          (int2Float (alC `div` 2) +)
          (primEsquina esquinasFondoAnimacion)
  let posicionesCelulas = [(px + int2Float x, py - int2Float y) | y <- [0, alC .. alC * (numCels - 1)], x <- [0, anC .. anC * (numCels - 1)]]
  let esquinasCelulas = map (tamañoVentanaAesquinasDeCuadrado tamañoCelula (1, 1)) posicionesCelulas
  let indicesCelulas = [(x, y) | x <- [0, 1 .. numCels - 1], y <- [0, 1 .. numCels - 1]]
  let datosCelulas = resuelveCelulasAutomata indicesCelulas esquinasCelulas (int2Float anC, int2Float alC) aut

  -- Definición de los botones
  let esquinasBotonBack = (primEsquina (mallaBase !! 12), secEsquina (mallaBase !! 13), tercEsquina (mallaBase !! 12), cuartaEsquina (mallaBase !! 13))
  let datosElementoBotonBack = metaDatosElementoVacioFondoBlanco {datosNombre = botonBack, datosEsquinas = esquinasBotonBack}
  let modulosBotonBack@(mbbAn, mbbAl) = moduloVectorEsquinas esquinasBotonBack
  let datosBotonBack =
        F.metadatosFormaBoton
          { F.metadatosElemento = datosElementoBotonBack,
            F.datosTexto = "Back to options",
            F.datosPorcentajesTexto = (0.05, 0.55),
            F.datosEscalasTexto = (0.2, 0.2),
            F.datosAltura = mbbAl / 2,
            F.datosAnchura = mbbAn
          }

  let esquinasBotonPause = (primEsquina (mallaBase !! 82), secEsquina (mallaBase !! 83), tercEsquina (mallaBase !! 82), cuartaEsquina (mallaBase !! 83))
  let datosElementoBotonPause = metaDatosElementoVacioFondoBlanco {datosNombre = botonPause, datosEsquinas = esquinasBotonPause}
  let modulosBotonPause@(mbpAn, mbpAl) = moduloVectorEsquinas esquinasBotonPause
  let datosBotonPause =
        F.metadatosFormaBoton
          { F.metadatosElemento = datosElementoBotonPause,
            F.datosTexto = "Pause",
            F.datosPorcentajesTexto = (0.1, 0.55),
            F.datosEscalasTexto = (0.2, 0.2),
            F.datosAltura = mbpAl / 2,
            F.datosAnchura = mbpAn
          }

  let esquinasBotonResume = (primEsquina (mallaBase !! 87), secEsquina (mallaBase !! 88), tercEsquina (mallaBase !! 87), cuartaEsquina (mallaBase !! 88))
  let datosElementoBotonResume = metaDatosElementoVacioFondoBlanco {datosNombre = botonPlay, datosEsquinas = esquinasBotonResume}
  let modulosBotonResume@(mbrAn, mbrAl) = moduloVectorEsquinas esquinasBotonResume
  let datosBotonResume =
        F.metadatosFormaBoton
          { F.metadatosElemento = datosElementoBotonResume,
            F.datosTexto = "Resume",
            F.datosPorcentajesTexto = (0.1, 0.55),
            F.datosEscalasTexto = (0.2, 0.2),
            F.datosAltura = mbrAl / 2,
            F.datosAnchura = mbrAn
          }

  -- Definición y construcción del almacén
  let datosAlmacen =
        DatosAlmacen
          { datosBarras = [],
            datosBases = [],
            datosEntradas = [],
            datosEtiquetas = [],
            datosFormas = [datosBotonBack, datosBotonPause, datosBotonResume] ++ datosCelulas,
            datosListas = [],
            datosParrafos = [],
            datosSaltosDeLinea = [],
            datosTitulos = []
          }
  return $ snd $ construyeAlmacen [] datosAlmacen

resuelveCelulasAutomata :: [Pos] -> [Esquinas] -> PosF -> Automata -> [F.DatosForma]
resuelveCelulasAutomata [] _ _ _ = []
resuelveCelulasAutomata _ [] _ _ = []
resuelveCelulasAutomata (i : indices) (e : esquinas) tam@(ancho, alto) automata
  | elementoPorPosicion automata i == 1 = datoViva : datos
  | otherwise = datos
  where
    datosElementoViva = metaDatosElementoVacioFondoBlanco {datosEsquinas = e}
    datoViva =
      F.metadatosFormaRectangulo
        { F.metadatosElemento = datosElementoViva,
          F.datosAltura = alto,
          F.datosAnchura = ancho
        }
    datos = resuelveCelulasAutomata indices esquinas tam automata

esperaAnimacion :: Mundo -> String -> IO Mundo
esperaAnimacion mundo nombreElPulsado
  | nombreElPulsado == botonBack = return $ mundo {siguientePantalla = opciones}
  | nombreElPulsado == botonPause = return $ mundo {M.animacion = False}
  | nombreElPulsado == botonPlay = return $ mundo {M.animacion = True}
  | otherwise = return mundo

animaAutomata :: Mundo -> IO Mundo
animaAutomata mundo
  | not anim = return mundo
  | length aut <= 1 = do
      semilla <- now
      let na
            | conds == etiquetaRandom = automataRandom semilla cels
            | otherwise = automataPreparado cels
      let mundoIntermedio = mundo {automata = na}
      mundoAutomataActualizado <- automataActualizado mundoIntermedio
      return $ mundoAutomataActualizado {fila = 1, M.animacion = True}
  | finalizado = do
      existeGuardado <- existe mundo
      if not existeGuardado && conds == etiquetaOneCell
        then do
          guardarAutomata mundo
          return $ mundo {M.animacion = False}
        else return $ mundo {M.animacion = False}
  | otherwise = do
      mundoAutomataActualizado <- automataActualizado mundo
      return $ mundoAutomataActualizado {fila = fil + 1}
  where
    conds = condiciones mundo
    aut = automata mundo
    anim = M.animacion mundo
    cels = celulas mundo
    fil = fila mundo
    finalizado = length aut <= fil

automataActualizado :: Mundo -> IO Mundo
automataActualizado mundo = do
  let guardado = automataGuardado mundo
  if length guardado > 1
    then do
      autActualizado <- cargarFilaArchivo mundo
      return mundo {automata = autActualizado}
    else do
      let reg = regla mundo
      let aut = automata mundo
      let fil = fila mundo
      let autActualizado = aplicaRegla fil reg aut
      return mundo {automata = autActualizado}
