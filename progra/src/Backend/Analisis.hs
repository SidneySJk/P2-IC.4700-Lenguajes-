{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Analisis where
import Backend.Importacion (Venta(..))

import qualified Data.ByteString.Lazy as DataJS
-- import Data.List.Split (splitOn)
import Data.Aeson (decode)
import Data.Maybe (fromMaybe)


totalVentas:: String -> IO ()      
totalVentas direccion = do 
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do 
            let totalCant = foldl(\sum x -> sum + total x) 0 v 
            putStrLn ("Total vendido: " ++ show totalCant)
        Nothing -> putStrLn "No se encontraron datos sobre la venta total"


split :: String -> [String] -- Fuente:  https://es.stackoverflow.com/questions/81221/c%C3%B3mo-transformar-un-string-a-una-lista-de-string-en-haskell
split "" = []
split xs = ys : (split . drop 1) zs
   where (ys, zs) = span (/='-') xs


totalVentasMensuales:: String -> String -> IO ()
totalVentasMensuales mes direccion = do 
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do
            let ventasMes = filter (\x -> 
                    case (split (fecha x)) of
                        [year, month, dia] -> month == mes
                        _ -> False ) v
            
            if null ventasMes then putStrLn "No hay registros del mes solicitado"
                else do 
                    let totalCant = foldl (\sum x -> sum + cantidad x) 0 ventasMes
                    putStrLn $ "Total vendido en el mes" ++ mes ++ ": " ++ show totalCant
        Nothing -> putStrLn "No se encontraron datos respecto a la venta total"


totalVentasAnuales:: String -> String -> IO ()
totalVentasAnuales anio direccion = do 
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do
            let ventasAnio = filter (\x -> 
                    case (split (fecha x)) of
                        [year, mes, dia] -> year == anio
                        _ -> False ) v
                           
            if null ventasAnio then putStrLn "No hay registros del año solicitado"
                else do 
                    let totalCant = foldl (\sum x -> sum + cantidad x) 0 ventasAnio
                    putStrLn $ "Total vendido en el año" ++ anio ++ ": " ++ show totalCant
        Nothing -> putStrLn "No se encontraron datos respecto a la venta total"

{-
mostrarVentasMensuales:: IO ()
mostrarVentasMensuales = do
    let meses = [1..12]
    let mostrar = map (\x -> (totalVentasMensuales x)) meses
-}   
