module Tipos.TipoAlmacenElementos
  ( AlmacenElementos (..),
    DatosAlmacen (..),
    almacenVacio,
    dibujaAlmacen,
    construyeAlmacen,
    extraeRelaciones,
    devuelveElementoPulsado,
    elementoReferido,
    posicionReferido,
    identidadElementoPulsado,
    nombreElementoPulsado,
    gestionaPulsacionElemento,
    gestionaActualizacionesPasivasElementos,
    gestionaTeclaChar,
    gestionaTeclaEspecial,
    distribuyeDistintasAccionesAutomaticamente,
  )
where

import Graphics.Gloss (Picture, pictures, white)
import Tipos.TipoBarraNavegacion as Barra
import Tipos.TipoBase (metadatosBaseVacia)
import Tipos.TipoBase as Base
import Tipos.TipoElemento as El
import Tipos.TipoEntrada as En
import Tipos.TipoEtiqueta as Et
import Tipos.TipoForma as F
import Tipos.TipoLista as L
import Tipos.TipoParrafo as P
import Tipos.TipoPosicion (Esquinas, Malla, PosF, posicionDentroDeEsquinas)
import Tipos.TipoSaltoDeLinea as S
import Tipos.TipoTitulo as T
import Utilidades.Utiles (listaIOs2IOlista)
import Graphics.Gloss.Interface.IO.Game (SpecialKey, Event (EventKey), Key (MouseButton, Char, SpecialKey), MouseButton (LeftButton), KeyState (Up))

data AlmacenElementos = AlmacenElementos
  { barrasNavegacion :: [BarraNavegacion],
    bases :: [Base],
    entradas :: [Entrada],
    etiquetas :: [Etiqueta],
    formas :: [Forma],
    listas :: [Lista],
    parrafos :: [Parrafo],
    saltosDeLinea :: [SaltoDeLinea],
    titulos :: [Titulo]
  }
  deriving (Show)

data DatosAlmacen = DatosAlmacen
  { datosBarras :: [DatosBarraNavegacion],
    datosBases :: [DatosBase],
    datosEntradas :: [DatosEntrada],
    datosEtiquetas :: [DatosEtiqueta],
    datosFormas :: [DatosForma],
    datosListas :: [DatosLista],
    datosParrafos :: [DatosParrafo],
    datosSaltosDeLinea :: [DatosSaltoDeLinea],
    datosTitulos :: [DatosTitulo]
  }
  deriving (Show)

almacenVacio :: AlmacenElementos
almacenVacio = AlmacenElementos {
    barrasNavegacion = [],
    bases = [],
    entradas = [],
    etiquetas = [],
    Tipos.TipoAlmacenElementos.formas = [],
    listas = [],
    parrafos = [],
    saltosDeLinea = [],
    titulos = []
}

-- Constantes
constanteIdentificador :: String
constanteIdentificador = "identificador"

constanteNombre :: String
constanteNombre = "nombre"

constanteIdPadre :: String
constanteIdPadre = "idPadre"
-- -------------------------

-- ----------------------------------------------------------------------------------------
-- Funciones para el dibujado de los elementos
-- ----------------------------------------------------------------------------------------
dibujaAlmacen :: AlmacenElementos -> Base -> IO Picture
dibujaAlmacen almacenados base = do
  let referidosAlmacenados = cotejaElementos almacenados (Base.idsElementosAlojados base) constanteIdentificador
  let barrasPintadas = map Barra.dibujaBarraNavegacion $ barrasNavegacion referidosAlmacenados
  let basesPintadas = map (dibujaAlmacen almacenados) $ bases referidosAlmacenados
  let entradasPintadas = map En.dibujaEntrada $ entradas referidosAlmacenados
  let etiquetasPintadas = map (\e -> Et.dibujaEtiqueta (posicionReferidoEtiqueta e) e) $ etiquetas referidosAlmacenados
  let formasPintadas = map (F.dibujaForma []) $ Tipos.TipoAlmacenElementos.formas referidosAlmacenados
  let listasPintadas = map L.dibujaLista $ listas referidosAlmacenados
  let parrafosPintados = map P.dibujaParrafo $ parrafos referidosAlmacenados
  let saltosDeLineaPintados = map S.dibujaSaltoDeLinea $ saltosDeLinea referidosAlmacenados
  let titulosPintados = map T.dibujaTitulo $ titulos referidosAlmacenados
  parte1 <- listaIOs2IOlista $ map listaIOs2IOlista [barrasPintadas, basesPintadas, entradasPintadas, etiquetasPintadas, formasPintadas]
  let dibujosParte1 = pictures $ map pictures parte1
  parte2 <- listaIOs2IOlista $ map listaIOs2IOlista [listasPintadas, parrafosPintados, saltosDeLineaPintados, titulosPintados]
  let dibujosParte2 = pictures $ map pictures parte2
  return $ pictures [dibujosParte2, dibujosParte1]
  where
    posicionReferidoEtiqueta e = posicionReferido almacenados $ Et.nombreElementoAsociado e

construyeAlmacen :: ListaIdentificadores -> DatosAlmacen -> (ListaIdentificadores, AlmacenElementos)
construyeAlmacen ids datosAlmacen =
  ( idsActualizados,
    AlmacenElementos
      { barrasNavegacion = brs,
        bases = bs,
        entradas = ens,
        etiquetas = ets,
        Tipos.TipoAlmacenElementos.formas = fs,
        listas = ls,
        parrafos = ps,
        saltosDeLinea = ss,
        titulos = ts
      }
  )
  where
    (idsBarras, brs) = Barra.construyeBarrasNavegacion ids $ datosBarras datosAlmacen
    (idsEntradas, ens) = En.construyeEntradas idsBarras $ datosEntradas datosAlmacen
    (idsEtiquetas, ets) = Et.construyeEtiquetas idsEntradas $ datosEtiquetas datosAlmacen
    (idsFormas, fs) = F.construyeFormas idsEtiquetas $ Tipos.TipoAlmacenElementos.datosFormas datosAlmacen
    (idsListas, ls) = L.construyeListas idsFormas $ datosListas datosAlmacen
    (idsParrafos, ps) = P.construyeParrafos idsListas $ datosParrafos datosAlmacen
    (idsSaltos, ss) = S.construyeSaltosDeLinea idsParrafos $ datosSaltosDeLinea datosAlmacen
    (idsTitulos, ts) = T.construyeTitulos idsSaltos $ datosTitulos datosAlmacen
    (idsActualizados, bs) = Base.construyeBases idsTitulos $ datosBases datosAlmacen

extraeRelaciones :: AlmacenElementos -> Base
extraeRelaciones almacen = snd $ construyeBase [] datosBase
  where
    idBarras = map (identificador . Barra.datosElemento) (barrasNavegacion almacen)
    idBases = map (identificador . Base.datosElemento) (Tipos.TipoAlmacenElementos.bases almacen)
    idEntradas = map (identificador . En.datosElemento) (Tipos.TipoAlmacenElementos.entradas almacen)
    idEtiquetas = map (identificador . Et.datosElemento) (Tipos.TipoAlmacenElementos.etiquetas almacen)
    idFormas = map (identificador . F.datosElemento) (Tipos.TipoAlmacenElementos.formas almacen)
    idListas = map (identificador . L.datosElemento) (Tipos.TipoAlmacenElementos.listas almacen)
    idParrafos = map (identificador . P.datosElemento) (Tipos.TipoAlmacenElementos.parrafos almacen)
    idSaltosDeLinea = map (identificador . S.datosElemento) (Tipos.TipoAlmacenElementos.saltosDeLinea almacen)
    idTitulos = map (identificador . T.datosElemento) (Tipos.TipoAlmacenElementos.titulos almacen)
    metadatos = metaDatosElementoVacioFondoBlanco {datosEsquinas = ((-150, 150), (150, 150), (-150, -150), (150, -150))}
    datosBase =
      metadatosBaseVacia
        { Base.metadatosElemento = metadatos,
          datosIdsElementosAlojados = idBarras ++ idBases ++ idEntradas ++ idEtiquetas ++ idFormas ++ idListas ++ idParrafos ++ idSaltosDeLinea ++ idTitulos
        }

-- ----------------------------------------------------------------------------------------
-- Funciones de utilidad
-- ----------------------------------------------------------------------------------------
-- Función auxiliar de cotejaElementos
dameFuncionDeElementoString :: String -> (Elemento -> String)
dameFuncionDeElementoString nombreFuncion
  | nombreFuncion == constanteIdentificador = El.identificador
  | nombreFuncion == constanteNombre = El.nombre
  | nombreFuncion == constanteIdPadre = El.idPadre
  | otherwise = error "Nombre de función no existente en dameFuncionDeElementoString"

cotejaElementos :: AlmacenElementos -> [String] -> String -> AlmacenElementos
cotejaElementos almacen idsBase nombreFuncion =
  AlmacenElementos
    { barrasNavegacion = bn,
      bases = bs,
      entradas = es,
      etiquetas = ets,
      Tipos.TipoAlmacenElementos.formas = fs,
      listas = ls,
      parrafos = ps,
      saltosDeLinea = ss,
      titulos = ts
    }
  where
    funcionUsada = dameFuncionDeElementoString nombreFuncion
    bn = filter (\e -> funcionUsada (Barra.datosElemento e) `elem` idsBase) $ barrasNavegacion almacen
    bs = filter (\e -> funcionUsada (Base.datosElemento e) `elem` idsBase) $ bases almacen
    es = filter (\e -> funcionUsada (En.datosElemento e) `elem` idsBase) $ entradas almacen
    ets = filter (\e -> funcionUsada (Et.datosElemento e) `elem` idsBase) $ etiquetas almacen
    fs = filter (\e -> funcionUsada (F.datosElemento e) `elem` idsBase) $ Tipos.TipoAlmacenElementos.formas almacen
    ls = filter (\e -> funcionUsada (L.datosElemento e) `elem` idsBase) $ listas almacen
    ps = filter (\e -> funcionUsada (P.datosElemento e) `elem` idsBase) $ parrafos almacen
    ss = filter (\e -> funcionUsada (S.datosElemento e) `elem` idsBase) $ saltosDeLinea almacen
    ts = filter (\e -> funcionUsada (T.datosElemento e) `elem` idsBase) $ titulos almacen

cotejaElementosPulsados :: AlmacenElementos -> AlmacenElementos
cotejaElementosPulsados almacen =
  AlmacenElementos
    { barrasNavegacion = bn,
      bases = bs,
      entradas = es,
      etiquetas = ets,
      Tipos.TipoAlmacenElementos.formas = fs,
      listas = ls,
      parrafos = ps,
      saltosDeLinea = ss,
      titulos = ts
    }
  where
    funcionUsada = El.pulsado
    bn = filter (\e -> funcionUsada (Barra.datosElemento e)) $ barrasNavegacion almacen
    bs = filter (\e -> funcionUsada (Base.datosElemento e)) $ bases almacen
    es = filter (\e -> funcionUsada (En.datosElemento e)) $ entradas almacen
    ets = filter (\e -> funcionUsada (Et.datosElemento e)) $ etiquetas almacen
    fs = filter (\e -> funcionUsada (F.datosElemento e)) $ Tipos.TipoAlmacenElementos.formas almacen
    ls = filter (\e -> funcionUsada (L.datosElemento e)) $ listas almacen
    ps = filter (\e -> funcionUsada (P.datosElemento e)) $ parrafos almacen
    ss = filter (\e -> funcionUsada (S.datosElemento e)) $ saltosDeLinea almacen
    ts = filter (\e -> funcionUsada (T.datosElemento e)) $ titulos almacen

devuelveElementoPorString :: AlmacenElementos -> String -> String -> [[Elemento]]
devuelveElementoPorString almacen nombre nombreFuncion = filter (not . null) [barras, bases, entradas, etiquetas, formas, listas, parrafos, saltosDeLinea, titulos]
  where
    referidosAlmacenados = cotejaElementos almacen [nombre] nombreFuncion
    barras = map Barra.datosElemento $ barrasNavegacion referidosAlmacenados
    bases = map Base.datosElemento $ Tipos.TipoAlmacenElementos.bases referidosAlmacenados
    entradas = map En.datosElemento $ Tipos.TipoAlmacenElementos.entradas referidosAlmacenados
    etiquetas = map Et.datosElemento $ Tipos.TipoAlmacenElementos.etiquetas referidosAlmacenados
    formas = map F.datosElemento $ Tipos.TipoAlmacenElementos.formas referidosAlmacenados
    listas = map L.datosElemento $ Tipos.TipoAlmacenElementos.listas referidosAlmacenados
    parrafos = map P.datosElemento $ Tipos.TipoAlmacenElementos.parrafos referidosAlmacenados
    saltosDeLinea = map S.datosElemento $ Tipos.TipoAlmacenElementos.saltosDeLinea referidosAlmacenados
    titulos = map T.datosElemento $ Tipos.TipoAlmacenElementos.titulos referidosAlmacenados

devuelveElementoPulsado :: AlmacenElementos -> Elemento
devuelveElementoPulsado almacen
  | not (null filtrados) = if not (null elementos) then head elementos else error "ERROR: No existe nigún elemento pulsado en el almacen proporcionado en devuelveElementoPulsado"
  | otherwise = error ("ERROR: No existen elementos que cumplan con el requisito 'pulsado' para el almacen proporcionado en devuelveElementoPulsado")
  where
    referidosAlmacenados = cotejaElementosPulsados almacen
    barras = map Barra.datosElemento $ barrasNavegacion referidosAlmacenados
    bases = map Base.datosElemento $ Tipos.TipoAlmacenElementos.bases referidosAlmacenados
    entradas = map En.datosElemento $ Tipos.TipoAlmacenElementos.entradas referidosAlmacenados
    etiquetas = map Et.datosElemento $ Tipos.TipoAlmacenElementos.etiquetas referidosAlmacenados
    formas = map F.datosElemento $ Tipos.TipoAlmacenElementos.formas referidosAlmacenados
    listas = map L.datosElemento $ Tipos.TipoAlmacenElementos.listas referidosAlmacenados
    parrafos = map P.datosElemento $ Tipos.TipoAlmacenElementos.parrafos referidosAlmacenados
    saltosDeLinea = map S.datosElemento $ Tipos.TipoAlmacenElementos.saltosDeLinea referidosAlmacenados
    titulos = map T.datosElemento $ Tipos.TipoAlmacenElementos.titulos referidosAlmacenados
    filtrados = filter (not . null) [barras, bases, entradas, etiquetas, formas, listas, parrafos, saltosDeLinea, titulos]
    elementos = head filtrados

elementoReferido :: AlmacenElementos -> String -> Elemento
elementoReferido almacen nombre
  | not (null filtrados) = if not (null elementos) then head elementos else error "ERROR: No existe el elemento referido en el almacen proporcionado en elementoReferido"
  | otherwise = error ("ERROR: No existen elementos que cumplan con el nombre otorgado para el almacen proporcionado en elementoReferido. Nombre otorgado: " ++ nombre)
  where
    filtrados = devuelveElementoPorString almacen nombre constanteNombre
    elementos = head filtrados

posicionReferido :: AlmacenElementos -> String -> Esquinas
posicionReferido almacen nombre
  | not (null filtrados) = if not (null elementos) then El.esquinas $ head elementos else error "ERROR: No existe el elemento referido en el almacen proporcionado en posicionReferido"
  | otherwise = error ("ERROR: No existen elementos que cumplan con el nombre otorgado para el almacen proporcionado en posicionReferido. Nombre otorgado: " ++ nombre)
  where
    filtrados = devuelveElementoPorString almacen nombre constanteNombre
    elementos = head filtrados

elementosCoincidentesPorEsquinas :: AlmacenElementos -> PosF -> [[Elemento]]
elementosCoincidentesPorEsquinas almacen raton = filter (not . null) [bn, bs, es, fs, ls, ps, ss, ts, ets]
  where
    funcionUsada = El.esquinas
    bn = map Barra.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (Barra.datosElemento e)) raton) $ barrasNavegacion almacen
    bs = map Base.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (Base.datosElemento e)) raton) $ bases almacen
    es = map En.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (En.datosElemento e)) raton) $ entradas almacen
    ets = map Et.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (Et.datosElemento e)) raton) $ etiquetas almacen
    fs = map F.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (F.datosElemento e)) raton) $ Tipos.TipoAlmacenElementos.formas almacen
    ls = map L.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (L.datosElemento e)) raton) $ listas almacen
    ps = map P.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (P.datosElemento e)) raton) $ parrafos almacen
    ss = map S.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (S.datosElemento e)) raton) $ saltosDeLinea almacen
    ts = map T.datosElemento $ filter (\e -> posicionDentroDeEsquinas (funcionUsada (T.datosElemento e)) raton) $ titulos almacen

identidadElementoPulsado :: AlmacenElementos -> PosF -> String
identidadElementoPulsado almacen raton
  | not (null filtrados) = if not (null elementos) then El.identificador $ head elementos else "Ninguno/0"
  | otherwise = "Ninguno/0"
  where
    filtrados = elementosCoincidentesPorEsquinas almacen raton
    elementos = head filtrados

nombreElementoPulsado :: AlmacenElementos -> PosF -> String
nombreElementoPulsado almacen raton
  | not (null filtrados) = if not (null elementos) then El.nombre $ head elementos else "Ninguno/0"
  | otherwise = "Ninguno/0"
  where
    filtrados = elementosCoincidentesPorEsquinas almacen raton
    elementos = head filtrados

gestionaPulsacionElemento :: AlmacenElementos -> PosF -> IO AlmacenElementos
gestionaPulsacionElemento almacen raton = do
    let identificadorElemento = identidadElementoPulsado almacen raton
    let banderaResetFormas = elem identificadorElemento $ map (El.identificador) $ map (F.datosElemento) $ Tipos.TipoAlmacenElementos.formas almacen
    let barras = map (Barra.pulsaElemento identificadorElemento) $ barrasNavegacion almacen
    let entradas = map (En.pulsaElemento identificadorElemento) $ Tipos.TipoAlmacenElementos.entradas almacen
    let etiquetas = map (Et.pulsaElemento identificadorElemento) $ Tipos.TipoAlmacenElementos.etiquetas almacen
    let formas = map (F.pulsaElemento identificadorElemento banderaResetFormas) $ Tipos.TipoAlmacenElementos.formas almacen
    let listas = map (L.pulsaElemento identificadorElemento) $ Tipos.TipoAlmacenElementos.listas almacen
    let parrafos = map (P.pulsaElemento identificadorElemento) $ Tipos.TipoAlmacenElementos.parrafos almacen
    let saltosDeLinea = map (S.pulsaElemento identificadorElemento) $ Tipos.TipoAlmacenElementos.saltosDeLinea almacen
    let titulos = map (T.pulsaElemento identificadorElemento) $ Tipos.TipoAlmacenElementos.titulos almacen
    let almacenActualizado = AlmacenElementos {
      barrasNavegacion = barras,
      Tipos.TipoAlmacenElementos.bases = Tipos.TipoAlmacenElementos.bases almacen,
      Tipos.TipoAlmacenElementos.entradas = entradas,
      Tipos.TipoAlmacenElementos.etiquetas = etiquetas,
      Tipos.TipoAlmacenElementos.formas = formas,
      Tipos.TipoAlmacenElementos.listas = listas,
      Tipos.TipoAlmacenElementos.parrafos = parrafos,
      Tipos.TipoAlmacenElementos.saltosDeLinea = saltosDeLinea,
      Tipos.TipoAlmacenElementos.titulos = titulos
    }
    return almacenActualizado

gestionaActualizacionesPasivasElementos :: AlmacenElementos -> IO AlmacenElementos
gestionaActualizacionesPasivasElementos almacen = do
    let entradas = map En.actualizaEntrada $ Tipos.TipoAlmacenElementos.entradas almacen
    let almacenActualizado = almacen {
      Tipos.TipoAlmacenElementos.entradas = entradas
    }
    return almacenActualizado

gestionaTeclaChar :: Char -> AlmacenElementos -> IO AlmacenElementos
gestionaTeclaChar tecla almacen = do
    let entradas = map (En.pulsaTeclaChar tecla) $ Tipos.TipoAlmacenElementos.entradas almacen
    let almacenActualizado = almacen {
      Tipos.TipoAlmacenElementos.entradas = entradas
    }
    return almacenActualizado

gestionaTeclaEspecial :: SpecialKey -> AlmacenElementos -> IO AlmacenElementos
gestionaTeclaEspecial tecla almacen = do
    let entradas = map (En.pulsaTeclaEspecial tecla) $ Tipos.TipoAlmacenElementos.entradas almacen
    let almacenActualizado = almacen {
      Tipos.TipoAlmacenElementos.entradas = entradas
    }
    return almacenActualizado

distribuyeDistintasAccionesAutomaticamente :: Event -> AlmacenElementos -> IO AlmacenElementos
distribuyeDistintasAccionesAutomaticamente evento almacenActual
  | EventKey (MouseButton LeftButton) Up _ raton <- evento = gestionaPulsacionElemento almacenActual raton
  | EventKey (Char tecla) Up _ _ <- evento = gestionaTeclaChar tecla almacenActual
  | EventKey (SpecialKey tecla) Up _ _ <- evento = gestionaTeclaEspecial tecla almacenActual
  | otherwise = return almacenActual