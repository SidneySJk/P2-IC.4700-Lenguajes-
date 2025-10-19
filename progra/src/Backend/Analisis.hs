{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Analisis where
import Backend.Importacion (Venta(..))

import qualified Data.ByteString.Lazy as DataJS
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


totalVentasMensuales:: String -> String -> IO Int--IO ()
totalVentasMensuales mes direccion = do 
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do
            let ventasMes = filter (\x -> 
                    case (split (fecha x)) of
                        [_, month, _] -> month == mes
                        _ -> False ) v
            
            if null ventasMes then return 0 --() --putStrLn "No hay registros del mes solicitado"
                else do 
                    let totalCant = foldl (\sum x -> sum + cantidad x) 0 ventasMes
                    return totalCant
                    -- putStrLn $ "Total vendido en el mes " ++ mes ++ ": " ++ show totalCant
        Nothing -> return 0
            --putStrLn "No se encontraron datos respecto a la venta total"
            

{-
totalVentasAnuales:: String -> String -> IO ()
totalVentasAnuales anio direccion = do 
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do
            let ventasAnio = filter (\x -> 
                    case (split (fecha x)) of
                        [year, _, _] -> year == anio
                        _ -> False ) v
                           
            if null ventasAnio then ()--putStrLn "No hay registros del año solicitado"
                else do 
                    let totalCant = foldl (\sum x -> sum + cantidad x) 0 ventasAnio
                    -- return totalCant
                    putStrLn $ "Total vendido en el año " ++ anio ++ ": " ++ show totalCant
        Nothing -> putStrLn "No se encontraron datos respecto a la venta total"
-}

mostrarVentasMensuales:: String -> IO ()
mostrarVentasMensuales direccion = do
    let meses = [ if n < 10 then "0" ++ show n else show n | n <- [1..12] ] -- map show [01..12]
    mapM_ (\x -> do 
        total <- (totalVentasMensuales x direccion)
        putStrLn $ "Total vendido en el mes " ++ x ++ ": " ++ show (total)
        ) meses


totalVentasAnuales:: String -> String -> IO ()
totalVentasAnuales anio direccion = do 
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do
            let ventasAnio = filter (\x -> 
                    case (split (fecha x)) of
                        [year, _, _] -> year == anio
                        _ -> False ) v
                           
            if null ventasAnio then return ()--putStrLn "No hay registros del año solicitado"
                else do 
                    let totalCant = foldl (\sum x -> sum + cantidad x) 0 ventasAnio
                    putStrLn $ "Total vendido en el año " ++ anio ++ ": " ++ show totalCant
        Nothing -> putStrLn "No se encontraron datos respecto a la venta total"


mostrarVentasAnuales:: String -> IO()
mostrarVentasAnuales direccion = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]  
    case contenido of
        Just v -> do
             mapM_ (\x -> case (split (fecha x)) of
                            [year, _ , _] -> totalVentasAnuales year direccion
                            _ -> return () ) v
            
        Nothing -> putStrLn "No hay fechas registradas"