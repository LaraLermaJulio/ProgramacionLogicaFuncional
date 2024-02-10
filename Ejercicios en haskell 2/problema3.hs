calificar :: [Int] -> [String]
calificar notas = map asignarCalificacion notas
  where
    asignarCalificacion nota
      | nota >= 95 = "Excelente"
      | nota >= 85 = "Notable"
      | nota >= 75 = "Bueno"
      | nota >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"

-- ejemplo de uso:
main :: IO ()
main = do
  let notas = [92, 78, 88, 65, 100, 72]
      calificaciones = calificar notas
  putStrLn "Notas originales:"
  print notas
  putStrLn "Calificaciones correspondientes:"
  print calificaciones
