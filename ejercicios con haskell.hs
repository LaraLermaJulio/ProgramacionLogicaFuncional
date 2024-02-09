import Data.Char (toUpper)
--Problema 1
descuento :: Double -> Double -> Double
descuento precio descuentoAplicado =
    precio - ((precio * descuentoAplicado) / 100)

iva :: Double -> Double -> Double 
iva precio descuentoIVA = 
    precio + (precio * descuentoIVA / 100)

precioFinal :: [(Double, Double)] -> (Double -> Double -> Double) -> Double
precioFinal [] _ = 0
precioFinal ((precio, desc):resto) funcion =
    funcion precio desc + precioFinal resto funcion

lista = [(1000, 10), (50, 20)]
precioFinalConDescuento = precioFinal lista descuento
precioFinalConIVA = precioFinal lista iva
-- imprimir precioFinalConDescuento y/o precioFinalConIVA


--Problema 2
funcionLista :: (a -> b) -> [a] -> [b]
funcionLista _ [] = []
funcionLista f (x:xs) = f x : funcionLista f xs 

cuadrado :: Num a => a -> a
cuadrado n = n * n


--Problema 3
separarContarPalabras :: String -> [(String, Int)]
separarContarPalabras salida = map (\w -> (w, length w)) (words salida)


--Problema 4
escribirCalificacion :: Double -> String
escribirCalificacion calificacion
    |   calificacion >= 95 = "Excelente"
    |   calificacion >= 85 = "Notable"
    |   calificacion >= 75 = "Bueno"
    |   calificacion >= 70 = "Suficiente"
    |   otherwise = "Desempeño insuficiente"

calificaciones :: [(String, Double)] -> [(String, String)]
calificaciones = map (\(asignatura, calificacion) -> (map toUpper asignatura, escribirCalificacion calificacion))

--ejemplo
calcularCalificaciones = calificaciones [("Programacion", 80), ("Mate", 92) ,("Calculo",57)]



--Problema 5
moduloVector :: Floating a => [a] -> a
moduloVector = sqrt . sum . map (^2)



--Problema 6
media :: (Fractional a) => [a] -> a
media xs = sum xs / fromIntegral (length xs)

desviacionEstandar :: (Floating a) => [a] -> a
desviacionEstandar xs =
  let m = media xs
      n = fromIntegral $ length xs
      sumSquaredDiffs = sum $ map (\x -> (x - m) ^ 2) xs
  in sqrt (sumSquaredDiffs / n)

atipico :: (Floating a, Ord a) => [a] -> a -> Bool
atipico muestra n =
  let mediaMuestra = media muestra
      desviacion = desviacionEstandar muestra
      puntuacion = abs ((n - mediaMuestra) / desviacion)
  in puntuacion > 3

datosAtipicos :: (Floating a, Ord a) => [a] -> [a]
datosAtipicos muestra = filter (\x -> atipico muestra x) muestra
