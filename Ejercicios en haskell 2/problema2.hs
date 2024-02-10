
filtrarPorCondicion :: (a -> Bool) -> [a] -> [a]
filtrarPorCondicion _ [] = [] 
filtrarPorCondicion f (x:xs)
  | f x = x : filtrarPorCondicion f xs  
  | otherwise = filtrarPorCondicion f xs  

numerosPares :: [Int] -> [Int]
numerosPares = filtrarPorCondicion even

-- ejemplo de uso:
main :: IO ()
main = do
  let listaOriginal = [1, 2, 3, 4, 5, 6, 7, 8, 9]
      funcionBooleana = (> 3) 
      listaFiltrada = filtrarPorCondicion funcionBooleana listaOriginal
  putStrLn "Lista original:"
  print listaOriginal
  putStrLn "Lista filtrada:"
  print listaFiltrada
