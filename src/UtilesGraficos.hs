module UtilesGraficos(
    gris,
) where

import Graphics.Gloss

gris :: Color
gris = makeColor 0.4 0.4 0.4 0.8

marron :: Color
marron = makeColorI 140 76 0 255

tamCheckbox :: Float
tamCheckbox = 10.0

anchoBoton :: Float
anchoBoton = 150.0

altoBoton :: Float
altoBoton = 40.0

texto :: String -> Picture
texto = scale 0.2 0.2 . color black . text

menuInicial :: Mundo
menuInicial = ("menu", (0,"nada", vacia), False, [["nada"]])

iniciaOpciones :: Mundo
iniciaOpciones = ("opciones", (0, "nada", vacia), False, [["nada"]])

