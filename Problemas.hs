--1. **Suma de elementos en una lista:**
--   - Definir una función `sumarLista :: [Int] -> Int` que sume todos los elementos de una lista de enteros.
sumarLista :: [Int] -> Int
sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs


--2. **Factorial:**
--   - Implementar la función `factorial :: Int -> Int` que calcule el factorial de un número.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


--3. **Números pares:**
--   - Escribir una función `numerosPares :: Int -> [Int]` que genere una lista de números pares hasta el número dado.
numerosPares :: Int -> [Int]
numerosPares n | n < 2 = []
               | even n = numerosPares (n-2) ++ [n]
               | otherwise = numerosPares (n-1)


--4. **Longitud de una cadena:**
--   - Crear una función `longitudCadena :: String -> Int` que devuelva la longitud de una cadena.
longitudCadena :: String -> Int
longitudCadena [] = 0
longitudCadena (_:xs) = 1 + longitudCadena xs


--5. **Reverso de una lista:**
--   - Definir una función `reversoLista :: [a] -> [a]` que invierta una lista.
reversoLista :: [a] -> [a]
reversoLista [] = []
reversoLista (x:xs) = reversoLista xs ++ [x]

--6. **Duplicar elementos:**
--   - Escribir una función `duplicarElementos :: [Int] -> [Int]` que duplique cada elemento de una lista de enteros.
duplicarElementos :: [Int] -> [Int]
duplicarElementos [] = []
duplicarElementos (x:xs) = x : x : duplicarElementos xs


--7. **Filtrar elementos pares:**
--   - Crear una función `filtrarPares :: [Int] -> [Int]` que filtre los elementos pares de una lista.
filtrarPares :: [Int] -> [Int]
filtrarPares [] = []
filtrarPares (x:xs)
    | even x = x : filtrarPares xs
    | otherwise = filtrarPares xs


--8. **Fibonacci:**
--   - Implementar una función `fibonacci :: Int -> Int` que devuelva el n-ésimo número de la secuencia de Fibonacci.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


--9. **Divisores de un número:**
--   - Escribir una función `divisores :: Int -> [Int]` que devuelva la lista de divisores de un número.
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]


--10. **Palíndromo:**
--   - Definir una función `esPalindromo :: String -> Bool` que determine si una cadena es un palíndromo.
esPalindromo :: String -> Bool
esPalindromo str = str == reverse str