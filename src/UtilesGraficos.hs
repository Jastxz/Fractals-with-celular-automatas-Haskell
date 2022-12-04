module UtilesGraficos(
    -- Colores
    gris,
    marron,
    -- Constantes
    tamCheckbox,
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
    menuInicial,
    iniciaOpciones,
    listaTextos,
    dibujaCheckbox,
    boton,
    pulsaBox,
    cercaBox,
    pulsaCerca,
) where

import Graphics.Gloss
import Tipos
import Utiles

gris :: Color
gris = makeColor 0.4 0.4 0.4 0.8

marron :: Color
marron = makeColorI 140 76 0 255

tamCheckbox :: Float
tamCheckbox = 10.0

posListaDePropiedades :: Float
posListaDePropiedades = -140.0

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
texto = scale 0.2 0.2 . color black . text

menuInicial :: Mundo
menuInicial = ("menu", (0,"nada", vacia), False, [["nada"]])

iniciaOpciones :: Mundo
iniciaOpciones = ("opciones", (0, "nada", vacia), False, [["nada"]])

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
    fondo = color (dark green) (rectangleSolid an al)
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
cuadroRelleno :: Picture
cuadroRelleno = color black $ rectangleSolid tamCheckbox tamCheckbox

cuadroVacio :: Picture
cuadroVacio = rectangleWire tamCheckbox tamCheckbox
