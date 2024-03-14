module FizzBuzz where

import Data.List (intercalate)
-- Función para verificar si un número es primo
esPrimo :: Int -> Bool
esPrimo n
    -- Si n es menor o igual a 1, no es primo
    | n <= 1 = False
    -- Si no, se verifica si n es divisible por algún número entre 2 y la raíz cuadrada de n
    | otherwise = all (\x -> n `mod` x /= 0) [2..isqrt n]
    -- Función auxiliar para calcular la raíz cuadrada
    where isqrt = floor . sqrt . fromIntegral

-- Función para convertir un número entero a una cadena de texto
numeroAString :: Int -> String
numeroAString n
    -- Casos para números menores que 20
    | n < 20 = if n < 10 then unidades !! n else decenasEspeciales !! (n - 10)
    -- Caso especial para 100
    | n == 100 = "cien"
    -- Casos para números entre 20 y 100
    | n < 100 = if n >= 20 && n < 30
                    -- Caso especial para los "veiti-"
                    then let (d, r) = n `divMod` 10
                             decenaStr = if r /= 0 then "veinti" else decenas !! d
                         in decenaStr ++ restoveinti r
                    else let (d, r) = n `divMod` 10
                             decenaStr = if d == 2 && r /= 0 then "veinti" else decenas !! d
                         in if r == 0 then decenaStr else decenaStr ++ " y" ++ resto r
    -- Casos para números entre 100 y 1000
    | n < 1000 = let (c, r) = n `divMod` 100 in centenas !! c ++ resto r
    -- Casos para números emenores a 1 millon
    | n < 1000000 = let (m, r) = n `divMod` 1000 in 
                    if m == 1
                        then if r == 0 
                            then "mil" 
                            else "un mil " ++ resto r 
                        else if r == 0 
                            then numeroAString2 m ++ " mil"
                            else numeroAString2 m ++ " mil" ++ resto r
    -- Caso especial para 1.000.000
    | n == 1000000 = "Un millon"
    | otherwise = error "Número fuera de rango"

    -- Listas predefinidas para las unidades, decenas, centenas y casos especiales
    where unidades = ["cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve"]
          decenasEspeciales = ["diez", "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"]
          decenas = ["", "", "veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]
          centenas = ["", "ciento", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]
          -- Funciones auxiliares para convertir las unidades y decenas restantes
          restoveinti x = if x == 0 then "" else "" ++ numeroAString x
          resto x = if x == 0 then "" else " " ++ numeroAString x


-- Función para permitir la diferenciacion de "un" y "uno"
numeroAString2 :: Int -> String
numeroAString2 n
    -- Casos para números menores que 20
    | n < 20 = if n < 10 then unidades !! n else decenasEspeciales !! (n - 10)
    -- Casos para números entre 20 y 100
    | n < 100 = if n >= 20 && n < 30
                    -- Caso especial para los "veiti-"
                    then let (d, r) = n `divMod` 10
                         in if r /= 0 
                                then "veinti" ++ unidades !! r
                                else decenas !! d
                    else let (d, r) = n `divMod` 10
                             decenaStr = if d == 2 && r /= 0 then "veinti" else decenas !! d
                             unidadStr = if r == 1 then "un" else unidades !! r
                         in if r == 0 then decenaStr else decenaStr ++ " y " ++ unidadStr
    -- Casos para números entre 100 y 1000
    | n < 1000 = let (c, r) = n `divMod` 100
                     centenaStr = centenas !! c
                 in centenaStr ++ resto r
    | otherwise = error "Número fuera de rango"

    -- Listas predefinidas para las unidades, decenas, centenas y casos especiales (esta remplazando uno con un)
    where unidades = ["cero", "un", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve"]
          decenasEspeciales = ["diez", "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"]
          decenas = ["", "", "veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]
          centenas = ["", "ciento", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]
          resto x = if x == 0 then "" else " " ++ numeroAString2 x

-- Definición de la función fizzBuzz
fizzBuzz :: Int -> String
fizzBuzz n
    | esPrimo n = "FizzBuzz!"
    | otherwise = numeroAString n

main :: IO ()
main = do
    -- Lee una línea de entrada desde la consola
    input <- getLine
    -- Convierte la entrada a un número entero
    let numero = read input :: Int
    if numero >= 0 && numero <= 1000000
        then putStrLn $ fizzBuzz numero
        else putStrLn "Número fuera de rango."