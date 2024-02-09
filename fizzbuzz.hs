module FizzBuzz where

lessThan20 :: Int -> String
lessThan20 n
    | n > 0 && n < 20 =
        let answers = words ("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")
        in answers !! (n - 1)
    | otherwise = error "fuera de rango"

teens :: Int -> String
teens n
    | n >= 2 && n <= 9 = answers !! (n - 2)
    | otherwise = error "fuera de rango"
    where
        answers = words("twenty thirty forty fifty sixty seventy eighty ninety")

number :: Int -> String
number n 
    | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz" 
    | n `mod` 3 == 0 = "Fizz"
    | n `mod` 5 == 0 = "Buzz"
    | n < 20 = lessThan20 n 
    | n < 100 && n `mod` 10 == 0 = teens (n `div` 10)
    | n < 100 = teens (n `div` 10) ++ " " ++ lessThan20 (n `mod` 10)
    | otherwise = error "fuera de rango"