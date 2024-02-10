import Data.Char (toUpper)

asignarCalificacion :: Int -> String
asignarCalificacion nota
  | nota >= 95 = "Excelente"
  | nota >= 85 = "Notable"
  | nota >= 75 = "Bueno"
  | nota >= 70 = "Suficiente"
  | otherwise = "Desempeño insuficiente"

calificacionesAprobadas :: [(String, Int)] -> [(String, String)]
calificacionesAprobadas = map (\(asignatura, nota) -> (map toUpper asignatura, asignarCalificacion nota))

notasAsignaturas :: [(String, Int)]
notasAsignaturas =
  [ ("mate", 92),
    ("calculo", 78),
    ("fisica", 20)
  ]

-- ejemplo de uso:
main :: IO ()
main = do
  let calificacionesFinales = calificacionesAprobadas notasAsignaturas
  putStrLn "Calificaciones finales:"
  print calificacionesFinales
