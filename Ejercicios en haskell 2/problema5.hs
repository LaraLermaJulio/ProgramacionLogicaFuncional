-- inmueble(objeto)
data Inmueble = Inmueble
  { año :: Int,
    metros :: Int,
    habitaciones :: Int,
    garaje :: Bool,
    zona :: Char
  }
  deriving (Show)

-- calcular el precio de  inmueble
calcularPrecio :: Inmueble -> Double
calcularPrecio inmueble
  | zona inmueble == 'A' = precioBase * (1 - fromIntegral (añoActual - año inmueble) / 100)
  | zona inmueble == 'B' = precioBase * (1 - fromIntegral (añoActual - año inmueble) / 100) * 1.5
  | otherwise = error "Zona no válida"
  where
    precioBase = fromIntegral (metros inmueble * 1000 + habitaciones inmueble * 5000 + if garaje inmueble then 15000 else 0)
    añoActual = 2024

-- filtrar inmuebles
filtrarPorPresupuesto :: [Inmueble] -> Double -> [Inmueble]
filtrarPorPresupuesto inmuebles presupuesto = filter (\i -> calcularPrecio i <= presupuesto) inmuebles

-- ejemplo de uso:
main :: IO ()
main = do
  let listaInmuebles =
        [ Inmueble 2000 100 3 True 'A',
          Inmueble 2012 60 2 True 'B',
          Inmueble 1980 120 4 False 'A',
          Inmueble 2005 75 3 True 'B',
          Inmueble 2015 90 2 False 'A'
        ]
      presupuestoDeseado = 100000
      inmueblesAprobados = filtrarPorPresupuesto listaInmuebles presupuestoDeseado
  putStrLn "Inmuebles aprobados:"
  mapM_ print inmueblesAprobados
