module Opciones
  ( pintaOpciones,
    seleccionaOpciones,
  )
where

import Graphics.Gloss
import Tipos
import Utiles
import UtilesGraficos

pintaOpciones :: Mundo -> IO Picture
pintaOpciones mundo@(pantalla, (regla, condicion, automata), animacion, adicional) = do
  -- Inicializando algunas variables necesarias
  let [infoCon, infoReg, infoCel] = infoEstatica
  let evolucionCasillas = snd distribucionOpciones
  let evCReglas = evolucionCasillas - 60.0
  -- Receptáculo para mostrar las opciones
  let borde = uncurry rectangleWire tamañoRectangulo
  -- Dibujando las condiciones
  let alturasCon = [head alturasCasillas, alturasCasillas !! 1, alturasCasillas !! 2]
  let cond
        | condicion == "Random" = 0
        | otherwise = 1
  bloqueCon <- creaBloque alturasCon etiquetaCond infoCon cond distribucionOpciones
  -- Dibujando las reglas disponibles
  let alturasReglas = [alturasCasillas !! 3, alturasCasillas !! 4, alturasCasillas !! 5]
  bloqueReglas <- creaBloque alturasReglas etiquetaReglas infoReg regla (fst distribucionOpciones, evCReglas)
  -- Dibujando los tamaños de dibujado disponibles
  let alturasCel = [alturasCasillas !! 6, alturasCasillas !! 7, alturasCasillas !! 8]
  let cel = cabeza "pintaOpciones" $ cabeza "pintaOpciones" adicional
  let celula
        | cel == "Very big" = 3
        | cel == "Big" = 2
        | cel == "Standard" = 1
        | otherwise = 0
  bloqueCel <- creaBloque alturasCel etiquetaCel infoCel celula distribucionOpciones
  -- Preparamos los botones
  prop <- creaBotonExtraLargo posProp etiquetaProp
  anim <- creaBotonMedio posAnim etiquetaAnim
  -- Devolvemos la imagen
  return $ pictures [borde, bloqueCon, bloqueReglas, bloqueCel, prop, anim]

seleccionaOpciones :: Point -> Mundo -> IO Mundo
seleccionaOpciones raton@(x,y) mundo = do
  -- Valores de separación entre las casillas de las opciones
  let iC = fst distribucionOpciones
  let eC = snd distribucionOpciones
  let evCReglas = eC - 60.0
  -- Buscando la casilla en cuestión
  let indice = filter (\(a, _) -> cercaBox y a) $ zip alturasEstaticas [0 ..]
  let ind
        | null indice = 99
        | otherwise = (snd . cabeza "opcionPulsada") indice
  let fila
        | null indice = cabeza "opcionPulsada" infoEstatica
        | otherwise = infoEstatica !! ind
  let limite = length fila
  let listaDistribuciones
        | ind == 1 = zip [iC, iC + evCReglas ..] [0 .. (limite - 1)]
        | otherwise = zip [iC, iC + eC ..] [0 .. (limite - 1)]
  let indice2 = filter (\(l, _) -> cercaBox x l) listaDistribuciones
  let columna
        | null indice2 = cabeza "opcionPulsada" fila
        | otherwise = fila !! (snd . cabeza "opcionPulsada") indice2
  let propiedades = pulsaCerca raton posProp
  let anim = pulsaCerca raton posAnim
  -- Cambiamos la información del juego a ejecutar y preparamos el tablero inicial
  nuevoMundo@(prop, (reg, cond, aut), an, ad) <- cambiaOpcion raton mundo ind columna
  let mundoAejecutar
        | propiedades = ("propiedades", (reg, cond, aut), an, ad)
        | anim = ("animacion", (reg, cond, aut), True, ad)
        | otherwise = nuevoMundo
  return mundoAejecutar

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

etiquetaCond :: String
etiquetaCond = "Initial conditions"

etiquetaReglas :: String
etiquetaReglas = "Choose rule"

etiquetaCel :: String
etiquetaCel = "Choose number of cells"

etiquetaProp :: String
etiquetaProp = "Properties of selected rule"

posProp :: Point
posProp = (-240.0, -180.0)

posAnim :: Point
posAnim = (200.0, -180.0)

distribucionOpciones :: Point
distribucionOpciones = (-450.0, 130.0)

ancho :: Float
ancho = 500.0 / 2.0

ajusteInicialMenu :: Float
ajusteInicialMenu = ancho / (2 * 8.0)

infoEstatica :: [[String]]
infoEstatica = [condiciones, reglas, celulas]
  where
    condiciones = ["Random", "One cell activated"]
    reglas = ["30", "73", "90", "105", "122", "124", "126", "150", "193", "195"]
    celulas = ["Small", "Standard", "Big", "Very big"]

alturasEstaticas :: [Float]
alturasEstaticas = [condiciones, reglas, cels]
  where
    condiciones = alturasCasillas !! 2
    reglas = alturasCasillas !! 5
    cels = alturasCasillas !! 8

alturasCasillas :: [Float]
alturasCasillas = [a, a - diferencia .. -150.0]
  where
    a = ancho - ajusteInicialMenu * 2
    diferencia = a / 5.0

cambiaOpcion :: Point -> Mundo -> Int -> String -> IO Mundo
cambiaOpcion raton mundo@(pantalla, (regla, condicion, automata), animacion, adicional) nivel opcion
  | nivel == 0 = do
    let nuevoMundo = (pantalla, (regla, opcion, automata), animacion, adicional)
    return nuevoMundo
  | nivel == 1 = do
    let nuevoMundo = (pantalla, (traduceRegla opcion, condicion, automata), animacion, adicional)
    return nuevoMundo
  | nivel == 2 = do
    let nuevoMundo = (pantalla, (regla, condicion, automata), animacion, [[opcion]])
    return nuevoMundo
  | nivel == 99 = return mundo
  | otherwise = error "El nivel de opciones especificado para la función cambiaOpción."

traduceRegla :: String -> Int
traduceRegla regla
  | regla == "30" = 0
  | regla == "73" = 1
  | regla == "90" = 2
  | regla == "105" = 3
  | regla == "122" = 4
  | regla == "124" = 5
  | regla == "126" = 6
  | regla == "150" = 7
  | regla == "193" = 8
  | regla == "195" = 9
  | otherwise = error $ "En traduceRegla no esta entrando una regla valida. Entrada: " ++ regla
