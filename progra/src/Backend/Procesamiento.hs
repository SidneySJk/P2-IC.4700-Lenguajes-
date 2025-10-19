{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Procesamiento where

import Backend.Importacion (Venta(..))

import Data.Aeson.Types (ToJSON(toJSON), FromJSON(parseJSON), Value(..), object, (.=), (.:), withObject, parseMaybe) 
import Data.Aeson (encode, decode, pairs)

-- Libreria Data.Aeson
import qualified Data.ByteString.Lazy as DataJS
import Data.Aeson.Encode.Pretty (encodePretty, defConfig, Config(..))
import Data.Text (Text)

import Data.Maybe (catMaybes)

{-| La funcion 'encontrarNullCantidad' determina si no hay cantidades '0' en las ventas.
 - Si no hay, retorna un comentario
 - Si hay, retorna la funcion 'modificarNullCantidad'
 - Toma un argumenot, de tipo 'Int'.
-}
encontrarNullCantidad :: String -> IO ()
encontrarNullCantidad direccion = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]
    case contenido of
        Just v -> do
            let esNullLista = map venta_id (filter(\x -> cantidad x == 0) v) -- let esNullLista = filter (\x -> if cantidad x == 0 then (venta_id x) else 0) v
            case esNullLista of
                [] -> putStrLn "Todas las cantidades tienen un dato"
                (x:_) -> modificarNullCantidad direccion (x)
                {-
                Just idVenta -> if idVenta /= 0 then (modificarNullPrecioUnitario direccion idVenta) else putStrLn "No se encontraron precios unitarios null"
                Nothing -> putStrLn "No se pudo extrar el id"
                -}
        Nothing -> putStrLn "Error al decodificar el JSON"


modificarNullCantidad:: String -> Int -> IO ()
modificarNullCantidad direccion idVenta = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]
    case contenido of 
        Just v -> do
            -- encontrarCantidad (total x) (precio_unitario x)}
            let totalCant = foldl(\sum x -> sum + cantidad x) 0 v
            let cantElem = length v
            let media = totalCant `div` cantElem
            let nullActualizado = map (\x -> 
                    if (venta_id x == idVenta) 
                    then x {cantidad = media}  
                    else x) v
            putStrLn $ "El se ha modificado el cantidad de la venta de id: " ++ show idVenta 
            DataJS.writeFile direccion (encodePretty nullActualizado)
        Nothing -> putStrLn "Error al decodificar el JSON"

encontrarCantidad :: Double -> Double -> Int
encontrarCantidad total precioUnitario = round (total / precioUnitario)


-- Encontrar precio unitario null

encontrarNullPrecioUnitario :: String -> IO()
encontrarNullPrecioUnitario direccion = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]
    case contenido of
        Just v -> do
            let esNullLista = map venta_id (filter(\x -> precio_unitario x == 0) v) -- filter (\x -> if precio_unitario x == 0 then (venta_id x) else 0) v
            case esNullLista of
                [] -> putStrLn "Todas los precios unitarios tienen un dato"
                (x:_) -> modificarNullPrecioUnitario direccion (x)
                {-
                Just idVenta -> if idVenta /= 0 then (modificarNullPrecioUnitario direccion idVenta) else putStrLn "No se encontraron precios unitarios null"
                Nothing -> putStrLn "No se pudo extrar el id"
                -}
        Nothing -> putStrLn "Error al decodificar el JSON"


modificarNullPrecioUnitario:: String -> Int -> IO ()
modificarNullPrecioUnitario direccion idVenta = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]
    case contenido of 
        Just v -> do
            -- encontrarPrecioUnitario (total x) (cantidad x)} 
            let totalCant = foldl(\sum x -> sum + precio_unitario x) 0 v
            let cantElem = length v
            let media = totalCant / fromIntegral cantElem
            let nullActualizado = map (\x -> 
                    if venta_id x == idVenta
                    then x {precio_unitario = media} 
                    else x) v
            putStrLn $ "El se ha modificado el precio unitario de la venta de id: " ++ show idVenta
            -- let actualizar = map (\x -> if venta_id x == id then x { precio_unitario = encontrarPrecioUnitario total cantidad } else x) v
            DataJS.writeFile direccion (encodePretty nullActualizado)
        Nothing -> putStrLn "Error al decodificar el JSON"


encontrarPrecioUnitario :: Double -> Int -> Double
encontrarPrecioUnitario total cantidad = total / fromIntegral cantidad 


eliminarId :: Int -> String -> IO ()--(Maybe Venta)
eliminarId idVenta direccion = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]
    case contenido of 
        Just v -> do
            let actualizado = filter (\x -> 
                    if ( venta_id x == idVenta ) 
                    then False 
                    else True) v
            -- let filtro = catMaybes actualizado
            DataJS.writeFile direccion (encodePretty actualizado) 


eliminarIdRepetido:: String -> IO ()
eliminarIdRepetido direccion = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]
    case contenido of 
        Just v -> do
            mapM_ (\x -> 
                if (length (filter (\y -> venta_id y == venta_id x) v) > 1) 
                    then do 
                        (eliminarId (venta_id x) direccion) 
                        putStrLn $ "Se encontro el id " ++ show(venta_id x) ++ " repetido, eliminando venta.."
                    else return()) v 
            --putStrLn $ "Se encontro el id " ++ show venta_id x ++ "repetido, eliminando venta.."