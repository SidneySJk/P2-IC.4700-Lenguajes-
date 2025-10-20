{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.AnalisisTemporal where
import Backend.Importacion (Venta(..))
import Backend.Analisis (totalVentasMensuales, split, )

import qualified Data.ByteString.Lazy as DataJS
import Data.Time
import Data.Time.Calendar 
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
            let cantidadMes = filter (\x -> do
                    case parseTimeM True defaultTimeLocale "%Y-%m-%d" (fecha x) :: Maybe Day of
                        Just d -> let (_ ,m ,_) = toGregorian (d) in show m == mes
                        Nothing -> False ) v
                    --case (split (fecha x)) of
                    --    [_, month, dia] -> (month == mes) 
                    --    _ -> False ) v
            let cantidadDia = map (\x -> 
                    case  parseTimeM True defaultTimeLocale "%Y-%m-%d" (fecha x) :: Maybe Day of
                        Just d -> (cantidad x, d) 
                        Nothing -> (0, fromGregorian 1883 1 1) ) cantidadMes
                        --[_, _, dia] -> (cantidad x, dia)
                        --_ -> (0, "Ninguno")) cantidadMes
            let maximo = maximum cantidadDia
            return (show(snd maximo))
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

totalVentas:: String -> String -> String ->  IO Int--IO ()
totalVentas mes anio direccion = do 
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]      
    case contenido of 
        Just v -> do
            let ventasMes = filter (\x -> 
                    case (split (fecha x)) of
                        [year, month, _] -> (month == mes && year == anio)
                        _ -> False ) v
            
            if null ventasMes then return 0 --() --putStrLn "No hay registros del mes solicitado"
                else do 
                    let totalCant = foldl (\sum x -> sum + cantidad x) 0 ventasMes
                    return totalCant
                    -- putStrLn $ "Total vendido en el mes " ++ mes ++ ": " ++ show totalCant
        Nothing -> return 0
            --putStrLn "No se encontraron datos respecto a la venta total"

determinarDia:: ([Int] -> Int) -> String -> String -> String -> IO Int
determinarDia f direccion anio mes = do
    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]
    case contenido of 
        Just v -> do
            let cantidadMes = filter (\x -> do
                    case parseTimeM True defaultTimeLocale "%Y-%m-%d" (fecha x) :: Maybe Day of
                        Just d -> let (y ,m ,_) = toGregorian (d) in (show m == mes && show y == anio)
                        Nothing -> False ) v
            let dias = map (\x -> 
                    case parseTimeM True defaultTimeLocale "%Y-%m-%d" (fecha x) :: Maybe Day of
                        Just d -> (d) 
                        Nothing -> (fromGregorian 1883 1 1) ) cantidadMes

            let resDias = map (\x -> let (_, _, d) = toGregorian x in d) dias
            let resultado = f (resDias)
            return (resultado)

        Nothing -> do
            putStrLn "No se pudo procesar la informacion"
            return 0 

determinarMes:: Day -> String
determinarMes fecha = 
    let (_, mes, _) = toGregorian fecha in show mes

determinarAnio:: Day -> String
determinarAnio fecha = 
    let (anio, _, _) = toGregorian fecha in show anio-- fromIntegral anio

obtenerMeses:: String -> String -> String -> IO Int--IO ()
obtenerMeses direccion anio mes = do
    diaUlt <- determinarDia maximum direccion anio mes
    let rangoUlt = fromGregorian (read anio) (read mes) (diaUlt) -- show anio ++ "-" ++ show mes ++ "-" ++ show day
    let intermedio = addGregorianMonthsClip (-1) rangoUlt
    let rangoIni = addGregorianMonthsClip (-2) rangoUlt
    
    let mesInter = determinarMes intermedio
    let mesIni = determinarMes rangoIni
    let anioInter = determinarAnio intermedio
    let anioIni = determinarAnio rangoIni

    mesUno <- determinarTrimestre direccion anio mes 
    mesDos <- determinarTrimestre direccion anioInter mesInter
    mesTres <- determinarTrimestre direccion anioIni mesIni

    let totalTrimestre = mesUno + mesDos + mesTres
    return totalTrimestre


determinarTrimestre:: String -> String -> String -> IO Int --In
determinarTrimestre direccion anio mes = do

    leer <- DataJS.readFile direccion
    let contenido = decode leer :: Maybe [Venta]
    case contenido of 
        Just v -> do
            --data <- mapM_ (x\ -> 
            mesTotal <- mapM (\x -> 
                case parseTimeM True defaultTimeLocale "%Y-%m-%d" (fecha x) :: Maybe Day of
                    Just d -> do
                        let (y, m, _) = toGregorian (d)
                        if (show m == mes && show y == anio)
                            then totalVentasMensuales (show m) direccion
                            else return 0
                        {-
                        let (y, m, _) = toGregorian (d) in (show m == mes && show y == anio)
                        return (totalVentasMensuales (m) (direccion))
                        -}
                    Nothing -> return (0)
                ) v
            return (sum mesTotal)
            
        Nothing -> do
            putStrLn "No se pudo procesar la informacion"
            return 0     
            --return data

aplicarFormula:: String -> String -> String -> IO Double
aplicarFormula direccion anio mes = do
    diaUlt <- determinarDia maximum direccion anio mes
    let rangoUlt = fromGregorian (read anio) (read mes) (diaUlt) -- show anio ++ "-" ++ show mes ++ "-" ++ show day
    let rangoIni = addGregorianMonthsClip (-2) rangoUlt
    let mesIni = determinarMes rangoIni
    let anioIni = determinarAnio rangoIni

    trimestreActual <- obtenerMeses direccion anio mes
    trimestrePasado <- obtenerMeses direccion (show anioIni) (show mesIni)

    return ((fromIntegral(trimestreActual - trimestrePasado) / fromIntegral trimestrePasado) * 100 :: Double)

