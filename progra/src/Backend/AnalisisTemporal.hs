{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.AnalisisTemporal where
import Backend.Importacion (Venta(..))
import Backend.Analisis (totalVentasMensuales, split, )

import qualified Data.ByteString.Lazy as DataJS
import Data.Aeson (decode)

{-| La funcion 'diaMasActivo' determina dado un mes, el dia con mas ventas.
 - Si no hay, retorna un comentario
 - Si hay, retorna el 'dia'
 - Toma dos argumentos, de tipo 'String'.
-}

diaMasActivo:: String -> String -> IO String
diaMasActivo mes direccion = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do 
            let cantidadMes = filter (\x -> 
                    case (split (fecha x)) of
                        [_, month, dia] -> (month == mes) 
                        _ -> False ) v
            let cantidadDia = map (\x -> 
                    case (split (fecha x)) of
                        [_, _, dia] -> (cantidad x, dia)
                        _ -> (0, "Ninguno")) cantidadMes
            let maximo = maximum cantidadDia
            return (snd maximo)
        Nothing -> return "No se pudo procesar la informacion"

{-| La funcion 'mesMayorVentaTotal' determina dado una direccion de archivo, el mes con mas ventas.
 - Retorna la informacion respecto:
    - Al mes con mayor venta total
    - Total vendido ese mes 
    - El dia mas activo del mes
 - Toma un argumentos, de tipo 'String'.
-}

mesMayorVentaTotal:: String -> IO ()
mesMayorVentaTotal direccion = do
    let meses = [ if n < 10 then "0" ++ show n else show n | n <- [1..12] ] 
    totales <- mapM (\x -> do
        total <- (totalVentasMensuales (x) (direccion))
        let resultado = (total, x)
        return resultado) meses

    let maximo = maximum totales
    dia <- diaMasActivo (snd maximo) direccion
    putStrLn (concat (replicate 20 "*--* ") ++ "\n") 
    putStrLn $ "El mes con mayor venta total es: " ++ snd maximo
    putStrLn $ "Total vendido en el mes: " ++ show (fst maximo) -- ++ "\n"
    putStrLn $ "Dia mas activo en el mes: " ++ show (dia) ++ "\n"
    putStrLn (concat (replicate 20 "*--* ") ++ "\n")


-- totalVentasMensuales mes direccion
-- determinarTrimestre 
