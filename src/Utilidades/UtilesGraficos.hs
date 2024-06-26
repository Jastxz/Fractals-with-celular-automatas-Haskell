module Utilidades.UtilesGraficos(
    -- Etiquetas
    etiquetaAnim,
    -- Constantes
    tamCheckbox,
    tamañoRectangulo,
    anchoBoton,
    altoBoton,
    anchoBotonMedio,
    altoBotonMedio,
    anchoBotonLargo,
    altoBotonLargo,
    anchoBotonExtraLargo,
    altoBotonExtraLargo,
    -- Funciones
    texto,
    creaBloque,
    creaBoton,
    creaBotonMedio,
    creaBotonExtraLargo,
    listaTextos,
    dibujaCheckbox,
    boton,
    pulsaBox,
    cercaBox,
    pulsaCerca,
) where

import Graphics.Gloss
import Tipos.TipoMundo
import Utilidades.Utiles

etiquetaAnim :: String
etiquetaAnim = "Watch animation"

tamCheckbox :: Float
tamCheckbox = 10.0

posListaDePropiedades :: Float
posListaDePropiedades = -430.0

horizontal :: Char
horizontal = 'X'

tamañoRectangulo :: Point
tamañoRectangulo = (1000, 500)

anchoBoton :: Float
anchoBoton = 150.0

altoBoton :: Float
altoBoton = 40.0

anchoBotonMedio :: Float
anchoBotonMedio = 250.0

altoBotonMedio :: Float
altoBotonMedio = 40.0

anchoBotonLargo :: Float
anchoBotonLargo = 350.0

altoBotonLargo :: Float
altoBotonLargo = 40.0

anchoBotonExtraLargo :: Float
anchoBotonExtraLargo = 450.0

altoBotonExtraLargo :: Float
altoBotonExtraLargo = 40.0

correccionPosicion :: Float -> Float
correccionPosicion ancho = ancho / 2.0

correccionPosicion2 :: Float -> Float
correccionPosicion2 ancho = ancho / 4.0

texto :: String -> Picture
texto = uncurry scale tamañoTexto . color black . text

creaBloque :: [Float] -> String -> [String] -> Int -> Point -> IO Picture
creaBloque alturas titulo info caja (inicioCasillas, evolucionCasillas) = do
  let [a1, a2, a3] = alturas
  let pintaTitulo = translate inicioCasillas a1 $ texto titulo
  let pintaInfo = translate 0 a2 $ pictures $ listaTextos info horizontal inicioCasillas evolucionCasillas False
  let lInfo = length info
  let cbx = pictures $ dibujaCheckbox (lInfo - 1) caja horizontal inicioCasillas evolucionCasillas
  let checkbox = translate 0 a3 cbx
  return $ pictures [pintaTitulo, pintaInfo, checkbox]

creaBoton :: Point -> String -> IO Picture
creaBoton (x, y) etiqueta = return $ translate x y $ boton etiqueta anchoBoton altoBoton

creaBotonMedio :: Point -> String -> IO Picture
creaBotonMedio (x, y) etiqueta = return $ translate x y $ boton etiqueta anchoBotonMedio altoBotonMedio

creaBotonExtraLargo :: Point -> String -> IO Picture
creaBotonExtraLargo (x, y) etiqueta = return $ translate x y $ boton etiqueta anchoBotonExtraLargo altoBotonExtraLargo

listaTextos :: [String] -> Char -> Float -> Float -> Bool -> [Picture]
listaTextos [] _ _ _ _ = []
listaTextos (t : ts) eje actual modificador menu
  | eje == 'X' || eje == 'x' = translate actual constante tx : siguiente
  | eje == 'Y' || eje == 'y' = translate constante actual tx : siguiente
  | otherwise = error "El eje especificado a la función listaTextos no es correcto"
  where
    siguiente = listaTextos ts eje (actual + modificador) modificador menu
    tx = texto t
    constante
      | menu = posListaDePropiedades
      | otherwise = 0

dibujaCheckbox :: Int -> Int -> Char -> Float -> Float -> [Picture]
dibujaCheckbox total elegido eje actual modificador = dibujaCheckbox' total elegido eje actual modificador 0

dibujaCheckbox' :: Int -> Int -> Char -> Float -> Float -> Int -> [Picture]
dibujaCheckbox' total elegido eje actual modificador acum
  | acum > total = []
  | eje == 'X' || eje == 'x' = translate actual 0 checkbox : siguiente
  | eje == 'Y' || eje == 'y' = translate 0 actual checkbox : siguiente
  | otherwise = error "El eje especificado a la función dibujaCheckbox no es correcto"
  where
    checkbox
      | acum == elegido = cuadroRelleno
      | otherwise = cuadroVacio
    siguiente = dibujaCheckbox' total elegido eje (actual + modificador) modificador (acum + 1)

boton :: String -> Float -> Float -> Picture
boton palabra an al = pictures [fondo, tx]
  where
    fondo = color white (rectangleSolid an al)
    tx = translate (- correccionPosicion an) (- correccionPosicion2 al) $ color white (texto palabra)

-- -----------------------------------------------------------------------------------------------------------------------
pulsaBox :: Point -> Point -> Bool
pulsaBox (x, y) (i, j)
  | cercaBox x i && cercaBox y j = True
  | otherwise = False

-- Aux
cercaBox :: Float -> Float -> Bool
cercaBox a b
  | resta <= 10.0 = True
  | otherwise = False
  where
    resta = distanciaEuclidea a b

-- -----------------------------------------------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------------------------------------------
pulsaCerca :: Point -> Point -> Bool
pulsaCerca (x, y) (i, j)
  | cercaX x i && cercaY y j = True
  | otherwise = False

-- Aux
cercaX :: Float -> Float -> Bool
cercaX a b
  | resta <= 150.0 = True
  | otherwise = False
  where
    resta = distanciaEuclidea a b

-- Aux
cercaY :: Float -> Float -> Bool
cercaY a b
  | resta <= 20.0 = True
  | otherwise = False
  where
    resta = distanciaEuclidea a b

-- -----------------------------------------------------------------------------------------------------------------------

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}
tamañoTexto :: Point
tamañoTexto = (0.2,0.2)

cuadroRelleno :: Picture
cuadroRelleno = color black $ rectangleSolid tamCheckbox tamCheckbox

cuadroVacio :: Picture
cuadroVacio = rectangleWire tamCheckbox tamCheckbox

textoBoton :: String -> Picture
textoBoton = uncurry scale tamañoTexto . color white . text