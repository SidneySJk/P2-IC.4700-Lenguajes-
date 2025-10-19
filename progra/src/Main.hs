module Main where
-- module Backend.Importacion (validarDireccion, validarDatos) where

import System.IO
import Control.Monad()
import Backend.Importacion (validarDireccion, validarDatos)
import Backend.Procesamiento (encontrarNullCantidad, encontrarNullPrecioUnitario, eliminarIdRepetido)
import Backend.Analisis (totalVentas, mostrarVentasMensuales, mostrarVentasAnuales)

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
            menuImportacion
            menu
        "2" -> do
            menuProcesamiento
        "3" -> do
            menuAnalisisDatos
        "4" -> do
            menuAnalisisTemporal
        "5" -> do
            menuEstadisticas
            menu
        "6" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opcion no valida, intente de nuevo."
            menu


validacionesRutas :: String -> String -> IO Bool
validacionesRutas ruta rutaD = do
    valido <- validarDireccion ruta
    validoD <- validarDireccion rutaD
    if (not valido || not validoD) then do 
        putStrLn "Direccion no valida, intente de nuevo. "
        return False
    else do
        putStrLn $ "Importar datos desde la ruta: " ++ ruta
        putStrLn $ "Guardar datos en la ruta: " ++ rutaD
        return True


menuImportacion :: IO ()
menuImportacion = do 
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Menu de importacion de datos"
    putStrLn "Aprete espacio para volver al menu"
    putStrLn "Ingrese la ruta destino del archivo a importar: "
    rutaD <- getLine
    putStrLn "Ingrese la ruta del archivo a importar: "
    ruta <- getLine
    if (rutaD == " " || ruta == " ")
        then menu 
        else do
            valido <- validacionesRutas ruta rutaD
            -- let validarDir = validarDireccion ruta
            -- let validarDirD = validarDireccion rutaD
            if (not valido)
                then do 
                    menuImportacion
                else do 
                    validarDatos ruta rutaD
                    putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout


menuProcesamiento :: IO ()
menuProcesamiento = do 
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Menu de procesamiento de datos"
    putStrLn "Aprete espacio para volver al menu"
    putStrLn "Ingrese la ruta del archivo a procesar: "

    direccion <- getLine
    if direccion == " "
        then menu 
        else do
            validarDir <- validarDireccion direccion
            if (not validarDir) 
                then do 
                    putStrLn "Direccion invalida"
                    menuProcesamiento
                else do
                    encontrarNullCantidad direccion
                    encontrarNullPrecioUnitario direccion
                    eliminarIdRepetido direccion
                    putStrLn "Completando y eliminando datos..."
                    menu
                    putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout


menuAnalisisDatos :: IO ()
menuAnalisisDatos = do
    putStrLn (concat (replicate 20 "°.*."))
    putStrLn "Menu de Analisis de datos"
    putStrLn "1. Total de ventas"
    putStrLn "2. Total de ventas mensuales y anuales"
    -- putStrLn "3. Total de ventas por categoria por año"
    putStrLn "4. Salir"
    opcion <- getLine
    putStrLn (concat (replicate 20 "°.*."))
    hFlush stdout
    case opcion of
        "1" -> do

            putStrLn "Total de ventas seleccionada"
            putStrLn "Ingrese la ruta del archivo a procesar: "
            ruta <- getLine
            if ruta == " "
                then menuAnalisisDatos 
                else do
                    valido <- validarDireccion ruta
                    if (not valido) 
                        then do 
                            putStrLn "Direccion invalida"
                            menuAnalisisDatos 
                        else do
                            totalVentas ruta
                            menuAnalisisDatos
            putStrLn (concat (replicate 20 "°.*."))
        "2" -> do
            
            putStrLn "Total de ventas mensuales y anuales seleccionada"
            putStrLn "Ingrese la ruta del archivo a procesar: "
            ruta <- getLine
            if ruta == " "
                then menuAnalisisDatos 
                else do
                    valido <- validarDireccion ruta
                    if (not valido) 
                        then do 
                            putStrLn "Direccion invalida"
                            menuAnalisisDatos 
                        else do
                            mostrarVentasMensuales ruta
                            mostrarVentasAnuales ruta
                            menuAnalisisDatos
            putStrLn (concat (replicate 20 "°.*."))
        {-
        "3" -> do
            putStrLn "Total de ventas por categoria por año seleccionada"
            -- Llamar a la funcion correspondiente
            menuAnalisisDatos
        -}
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

-- "src/Backend/Modules/Data.json"
-- "src/Backend/Modules/Importar.json"