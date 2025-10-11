module Main where
-- module Backend.Importacion (validarDireccion, validarDatos) where

import System.IO
import Control.Monad()
import Backend.Importacion (validarDireccion, validarDatos)

-- validarDireccion :: String -> Bool
-- validarDireccion = undefined

-- validarDatos :: String -> IO ()
-- validarDatos = undefined

menu :: IO ()
menu = do
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Bienvenido al menu principal"
    putStrLn "1. Importacion de datos"
    putStrLn "2. Procesamiento de datos"
    putStrLn "3. Analisis de datos" 
    putStrLn "4. Analisis temporal"
    putStrLn "5. Estadisticas"
    putStrLn "6. Salir"
    putStrLn (concat (replicate 20 "°.*."))
    putStr "Ingrese una opcion: "
    hFlush stdout

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Importacion de datos seleccionada"
            -- Llamar a la funcion correspondiente
            menuImportacion
            menu
        "2" -> do
            putStrLn "Procesamiento de datos seleccionada"
            -- Llamar a la funcion correspondiente
            menuProcesamiento
        "3" -> do
            putStrLn "Analisis de datos seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisDatos
        "4" -> do
            putStrLn "Analisis temporal seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisTemporal
        "5" -> do
            putStrLn "Estadisticas seleccionada"
            -- Llamar a la funcion correspondiente
            menuEstadisticas
            menu
        "6" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opcion no valida, intente de nuevo."
            menu

menuImportacion :: IO ()
menuImportacion = do 
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Menu de importacion de datos"
    putStrLn "Aprete espacio para volver al menu"
    putStrLn "Ingrese la ruta del archivo a importar: "
    ruta <- getLine
    if ruta == " "
        then menu 
        else do
            valido <- validarDireccion ruta
            if valido == False 
                then do 
                    putStrLn "Direccion no valida, intente de nuevo. "
                    menuImportacion
                else do 
                    validarDatos ruta
                    putStrLn $ "Importar datos desde la ruta: " ++ ruta
                    putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout

menuProcesamiento :: IO ()
menuProcesamiento = do 
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Menu de procesamiento de datos"
    putStrLn "Aprete espacio para volver al menu"
    putStrLn "Completando y eliminando datos..."

    salir <- getLine
    if salir == " "
        then menu 
        else do
            menu 
            putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout

menuAnalisisDatos :: IO ()
menuAnalisisDatos = do
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Menu de Analisis de datos"
    putStrLn "1. Total de ventas"
    putStrLn "2. Total de ventas mensuales y anuales"
    putStrLn "3. Total de ventas por categoria por año"
    putStrLn "4. Salir"
    opcion <- getLine
    putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout
    case opcion of
        "1" -> do
            putStrLn "Total de ventas seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisDatos
        "2" -> do
            putStrLn "Total de ventas mensuales y anuales seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisDatos
        "3" -> do
            putStrLn "Total de ventas por categoria por año seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisDatos
        "4" -> menu
        _   -> do
            putStrLn "Opcion no valida, intente de nuevo."
            menuAnalisisDatos


menuAnalisisTemporal :: IO ()
menuAnalisisTemporal = do
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Menu de Analisis temporal"
    putStrLn "1. Mes con mayor venta total"
    putStrLn "2. Calcular la tasa de las ventas"
    putStrLn "3. Resumen de ventas por trimestre"
    putStrLn "4. Salir"
    opcion <- getLine
    putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout
    case opcion of
        "1" -> do
            putStrLn " Mes con mayor venta total seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisTemporal
        "2" -> do
            putStrLn "Calcular la tasa de las ventas seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisTemporal
        "3" -> do
            putStrLn "Resumen de ventas por trimestre seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisTemporal
        "4" -> menu
        _   -> do
            putStrLn "Opcion no valida, intente de nuevo."
            menuAnalisisTemporal

menuBusqueda :: IO ()
menuBusqueda = do
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Menu de busqueda"
    putStrLn "Indique el rango de fechas (formato: AAAA-MM-DD)"
    putStrLn "Aprete espacio para volver al menu"

    rango1 <- getLine
    putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout
    if rango1 == " "
        then menu
        else do
            rango2 <- getLine
            if rango2 == " "
                then menu
                else do
                    putStrLn $ "Buscando datos entre el rango de fechas: " ++ rango1 ++ " y " ++ rango2
                    menuBusqueda
                    putStrLn (concat (replicate 20 "°.*."))

menuEstadisticas :: IO ()
menuEstadisticas = do
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Top 5 de categorías con mayores ventas (monto)"
    putStrLn "Producto más vendido (por cantidad)"
    putStrLn "Categoría con menor participación (cantidad)"
    putStrLn "Resumen general: "
   
    putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout
    
main :: IO ()
main = do
    menu