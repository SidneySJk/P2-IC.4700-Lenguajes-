{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Estadistica where
import Backend.Importacion (Venta(..))

import qualified Data.ByteString.Lazy as DataJS
import Data.Time
import Data.Time.Calendar 
import Data.Aeson (decode)
import Data.List (minimumBy)
import Data.Ord (comparing)


determinarParticipacion:: String -> IO String
determinarParticipacion direccion = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do 
            resultados <- mapM (\x -> do
                                total <- participacion (categoria x) direccion
                                return (categoria x, total)) v
            let minima = minimumBy (comparing snd) resultados
            return (show(fst minima))
        Nothing -> return "No se pudo procesar la informacion"

participacion:: String -> String -> IO Int
participacion cate direccion = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do 
            let categorias = filter (\x -> categoria x == cate) v
            let resultados = foldl (\sum x -> sum + cantidad x) 0 categorias
            return resultados 
        Nothing -> do 
            putStrLn "No se pudo procesar la informacion"
            return 0 