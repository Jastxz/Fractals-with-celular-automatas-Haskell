module Utilidades.Ficheros
  ( preparaDirectorios,
    cargarAutomata,
    guardarAutomata,
    cargarFilaArchivo,
    caminoArchivo,
    nombreArchivo,
    existe,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString.Lazy as B
import System.Directory
import System.Directory.Internal.Prelude (when)
import System.FilePath ((</>))
import Tipos.TipoAutomata
import Tipos.TipoMundo
import Utilidades.BBDD
import Utilidades.Utiles (intercambia)

preparaDirectorios :: IO ()
preparaDirectorios = do
  caminoFicheros <- directorioFicheros
  createDirectoryIfMissing False caminoFicheros

cargarAutomata :: Mundo -> IO Mundo
cargarAutomata mundo = do
  archivo <- caminoArchivo mundo
  existe <- doesFileExist archivo
  if not existe
    then do
      let nombreArchivoBBDD = nombreArchivo mundo
      existeBBDD <- existeRegistro nombreArchivoBBDD
      if existeBBDD
        then do
          contenido <- leerRegistro $ nombreArchivo mundo
          let guardado = convertirJsonEnAutomata contenido
          return mundo {automataGuardado = guardado}
        else error $ "Error al cargar el fichero. No existe: " ++ archivo
    else do
      contenido <- B.readFile archivo
      evaluate (force contenido)
      if B.null contenido
        then error $ "Archivo pasado a la funcion cargaAutomata vacio. Path: " ++ archivo
        else do
          let guardado = convertirJsonEnAutomata contenido
          return mundo {automataGuardado = guardado}

guardarEnBBDD :: String -> ByteString -> IO Bool
guardarEnBBDD nombreRegistro registro = insertaRegistro nombreRegistro registro

guardarAutomata :: Mundo -> IO ()
guardarAutomata mundo = do
  caminoFicheros <- directorioFicheros
  archivos <- listDirectory caminoFicheros
  archivo <- caminoArchivo mundo
  let aut = automata mundo
  let contenidoArchivo = convertirAutomataEnJson aut
  B.writeFile archivo contenidoArchivo
  seHaGuardadoEnBBDD <- guardarEnBBDD (nombreArchivo mundo) contenidoArchivo
  seHaCreado <- doesFileExist archivo
  if seHaCreado
    then do
      let mensaje | seHaGuardadoEnBBDD = "Saved file in local and DB successfully" | otherwise = "Saved local file successfully"
      print mensaje
    else do
      error $ "No se ha podido crear el guardado de partida con nombre: " ++ archivo

cargarFilaArchivo :: Mundo -> IO Automata
cargarFilaArchivo mundo = do
  let cargado = automataGuardado mundo
  let fil = fila mundo
  let aut = automata mundo
  let filaCargada = cargado !! fil
  return $ intercambia aut filaCargada fil

{- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Funciones auxiliares
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -}

directorioFicheros :: IO String
directorioFicheros = do
  directorioActual <- getCurrentDirectory
  return $ directorioActual </> "Precalculados"

caminoArchivo :: Mundo -> IO String
caminoArchivo mundo = do
  caminoFicheros <- directorioFicheros
  return $ caminoFicheros ++ "/" ++ (nombreArchivo mundo) ++ ".json"

nombreArchivo :: Mundo -> String
nombreArchivo mundo = show reg ++ "_" ++ show cels
  where
    reg = regla mundo
    cels = celulas mundo

existe :: Mundo -> IO Bool
existe mundo = do
  rutaArchivoLocal <- caminoArchivo mundo
  let nombreArchivoBBDD = nombreArchivo mundo
  existeLocal <- doesFileExist rutaArchivoLocal
  existeBBDD <- existeRegistro nombreArchivoBBDD
  if existeLocal
    then return True
    else return existeBBDD