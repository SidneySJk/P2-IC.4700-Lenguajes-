{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Importacion where

import Data.Aeson.Types (ToJSON(toJSON), FromJSON(parseJSON), Value(..), object, (.=), (.:), withObject) 
import Data.Aeson (encode, decode, pairs)
import qualified Data.ByteString.Lazy as DataJS
import Data.Aeson.Encode.Pretty (encodePretty, defConfig, Config(..))
import Data.Text (Text)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time (Day)
import System.Directory (doesFileExist)

import Data.Maybe (catMaybes)


data Venta = Venta {
    venta_id :: Int,
    fecha :: String,
    producto_id :: Int,
    producto_nombre :: String,
    categoria :: String,
    cantidad :: Int,
    precio_unitario :: Double,
    total :: Double
} deriving (Show) -- deriving (Show, Generic)

instance ToJSON Venta where
    toJSON (Venta vid f pid pno cat cant pre tot) = object
        [ "venta_id" .= vid,
          "fecha" .= f,
          "producto_id" .= pid,
          "producto_nombre" .= pno,
          "categoria" .= cat,
          "cantidad" .= cant,
          "precio_unitario" .= pre,
          "total" .= tot
        ]

instance FromJSON Venta where
    parseJSON = withObject "Venta" $ \v -> Venta
        <$> v .: "venta_id"
        <*> v .: "fecha"
        <*> v .: "producto_id"
        <*> v .: "producto_nombre"
        <*> v .: "categoria"
        <*> v .: "cantidad"
        <*> v .: "precio_unitario"
        <*> v .: "total"


-- validarDireccion :: String -> IO Bool
validarDireccion direccion = do
    existe <- doesFileExist direccion
    return existe


validarFecha :: String -> Bool
validarFecha fecha = do
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" fecha:: Maybe Day of
        Just _ -> True 
        Nothing -> False  


validarVenta :: Venta -> IO (Maybe Venta)
validarVenta venta = do
    if (venta_id venta < 0 || cantidad venta < 0 || precio_unitario venta < 0 || total venta < 0 || null (fecha venta) || null (producto_nombre venta) || null (categoria venta))
        then return Nothing
        else if not (validarFecha (fecha venta))
            then return Nothing 
            else do
                -- let division =  ((total venta) / (fromIntegral (cantidad venta) * precio_unitario venta) )
                --if (division /= 1) then 
                --    return Nothing 
                --else
                return (Just venta)


concatenarJsons :: String -> String -> IO()
concatenarJsons direccion destino = do 
    leerDir <- DataJS.readFile direccion
    let ventasDir = decode leerDir :: Maybe [Venta]
    leerDest <- DataJS.readFile destino
    let ventasDest = decode leerDest :: Maybe [Venta]
    
    case (ventasDest, ventasDir) of
        (Just vDest, Just vDir) -> do 
            let concatenado = (vDest ++ vDir)
            jsonCorregido <- mapM validarVenta concatenado
            let filtro = catMaybes jsonCorregido -- filter esValida jsonCorregido  
            DataJS.writeFile destino (encodePretty filtro) 
        _ -> putStrLn "Error no se pudo importar el archivo"

 
validarDatos :: String -> String -> IO ()
validarDatos direccion destino = do
    if direccion == destino 
        then do
            leer <- DataJS.readFile direccion
            let ventas = decode leer :: Maybe [Venta]
            case ventas of 
                Just v -> do 
                    jsonCorregido <- mapM validarVenta v
                    let filtro = catMaybes jsonCorregido 
                    DataJS.writeFile destino (encodePretty filtro) 
                Nothing -> putStrLn "Error archivo Json no cumple con las especificaciones"
        else do
            concatenarJsons direccion destino
            putStrLn "Informacion importada con exito"



