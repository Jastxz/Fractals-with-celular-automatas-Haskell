module Utilidades.BBDD
  ( autenticar,
    insertaRegistro,
    leerRegistro,
    existeRegistro,
  )
where

import Data.Bson as Bs
import Data.ByteString.Lazy as B
import Data.Text as T
import Database.MongoDB
import Prelude as P

-- Constantes de conexión
hostDir :: String
hostDir = "127.0.0.1"

username :: Username
username = T.pack ""

password :: Password
password = T.pack ""

database :: Database
database = T.pack "fractales"

coleccion :: Collection
coleccion = T.pack "registros"

campoName :: Text
campoName = T.pack "name"

campoRegistro :: Text
campoRegistro = T.pack "automata"

-- ----------------------------------------------

pipeConexion :: IO Pipe
pipeConexion = connect (host hostDir)

cerrarConexion :: Pipe -> IO ()
cerrarConexion pipe = close pipe

autenticar :: Action IO Bool
autenticar = auth username password

insertaRegistro :: String -> ByteString -> IO Bool
insertaRegistro nombreRegistro registro = do
  let registroBinario = Bs.val $ Bs.Binary $ B.toStrict registro
  pipe <- pipeConexion
  print "Insertando"
  valueBson <- access pipe master database $ insert coleccion [campoName =: nombreRegistro, campoRegistro =: registroBinario]
  cerrarConexion pipe
  return $ isNotEmpty valueBson

buscarRegistro :: String -> Action IO Document
buscarRegistro nombreRegistro = do
  maybeDoc <- findOne (select [campoName =: nombreRegistro] coleccion)
  return $ desempaquetaMaybeDoc maybeDoc

leerRegistro :: String -> IO ByteString
leerRegistro nombreRegistro = do
  pipe <- pipeConexion
  print "Leyendo"
  documento <- access pipe master database $ buscarRegistro nombreRegistro
  cerrarConexion pipe
  if P.null documento
    then return B.empty
    else return $ desempaquetaMaybeByteString $ Bs.cast' $ Bs.valueAt campoRegistro documento

existeRegistro :: String -> IO Bool
existeRegistro nombreRegistro = do
  pipe <- pipeConexion
  print "Buscando"
  documento <- access pipe master database $ buscarRegistro nombreRegistro
  cerrarConexion pipe
  return $ not $ P.null documento

desempaquetaMaybeByteString :: Maybe Bs.Binary -> B.ByteString
desempaquetaMaybeByteString (P.Just (Bs.Binary bs)) = B.fromStrict bs
desempaquetaMaybeByteString P.Nothing = B.empty

desempaquetaMaybeDoc :: Maybe Document -> Document
desempaquetaMaybeDoc (P.Just doc) = doc
desempaquetaMaybeDoc P.Nothing = []

isNotEmpty :: Bs.Value -> Bool
isNotEmpty (Bs.String s) = not (T.null s)
isNotEmpty (Bs.Array arr) = not (P.null arr)
isNotEmpty (Bs.Doc doc) = not (P.null doc)
isNotEmpty (Bs.Int32 _) = True
isNotEmpty (Bs.Int64 _) = True
isNotEmpty (Bs.Float _) = True
isNotEmpty (Bs.Bool _) = True
isNotEmpty (Bs.UTC _) = True
isNotEmpty (Bs.Null) = False
isNotEmpty _ = False