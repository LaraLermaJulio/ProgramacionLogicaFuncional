import Data.List (intercalate)
import Numeric (showFFloat)

funciones :: [(String, Double -> Double)]
funciones =
  [ ("seno", sin),
    ("coseno", cos),
    ("tangente", tan),
    ("exponencial", exp),
    ("logaritmo neperiano", log)
  ]

pedirFuncion :: IO String
pedirFuncion = do
  putStrLn "Funciones: seno, coseno, tangente, exponencial, logaritmo neperiano"
  putStrLn "Ingrese el nombre de la función que desea aplicar:"
  funcion <- getLine
  if funcion `elem` map fst funciones
    then return funcion
    else do
      putStrLn "Funcion no valida."
      pedirFuncion

pedirValor :: IO Int
pedirValor = do
  putStrLn "Ingrese un valor entero mayor que cero:"
  valor <- readLn
  if valor >= 1
    then return valor
    else do
      putStrLn "Valor no valido."
      pedirValor

calcularLista :: (Double -> Double) -> Int -> [Double]
calcularLista f n = map f [1 .. fromIntegral n]

mostrarTabla :: String -> [Double] -> IO ()
mostrarTabla funcion resultados = do
  putStrLn $ "Funcion: " ++ funcion
  putStrLn $ "Valor | Resultado"
  putStrLn $ replicate 15 '-'
  mapM_ (\(i, res) -> putStrLn $ show i ++ " | " ++ showFFloat (Just 5) res "") (zip [1 ..] resultados)

-- ejemplo de uso:
main :: IO ()
main = do
  funcion <- pedirFuncion
  valor <- pedirValor
  let resultados = calcularLista (snd (head (filter (\(nombre, _) -> nombre == funcion) funciones))) valor
  mostrarTabla funcion resultados
