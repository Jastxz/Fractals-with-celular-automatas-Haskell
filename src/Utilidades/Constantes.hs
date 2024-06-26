module Utilidades.Constantes (
    Constante,
    tamañoVentana,
    posicionVentana,
    menu,
    opciones,
    propiedades,
    animacion,
    Utilidades.Constantes.error,
    botonIniciaOpciones,
    botonPropiedades,
    botonPlay,
    botonBack,
    botonPause,
    etiquetaRandom,
    etiquetaOneCell,
    entradaRegla,
    entradaNumCels,
) where

import Graphics.Gloss

type Constante = String
type Constantes = [Constante]

tamañoVentana :: (Int, Int)
tamañoVentana = (1600, 900)

posicionVentana :: (Int, Int)
posicionVentana = (180, 70)

menu :: Constante
menu = "menu"

opciones :: Constante
opciones = "opciones"

propiedades :: Constante
propiedades = "propiedades"

animacion :: Constante
animacion = "animacion"

error :: Constante
error = "error"

botonIniciaOpciones :: String
botonIniciaOpciones = "botonIniciaOpciones"

botonPropiedades :: String
botonPropiedades = "boton propiedades"

botonPlay :: String
botonPlay = "boton play"

botonBack :: String
botonBack = "boton back"

botonPause :: String
botonPause = "boton pause"

etiquetaRandom :: String
etiquetaRandom = "random"

etiquetaOneCell :: String
etiquetaOneCell = "one cell"

entradaRegla :: String
entradaRegla = "entrada regla"

entradaNumCels :: String
entradaNumCels = "entrada número de células"
