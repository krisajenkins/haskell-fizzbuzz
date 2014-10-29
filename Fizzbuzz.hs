data FB = Fizz | Buzz | Fizzbuzz deriving (Show,Eq)

matcher :: Int -> Int -> Maybe FB
matcher 0 0 = Just Fizzbuzz
matcher 0 _ = Just Fizz
matcher _ 0 = Just Buzz
matcher _ _ = Nothing

veilOver :: Int -> Either FB Int
veilOver n = case matcher (mod n 3) (mod n 5) of
                Nothing -> Right n
                Just x  -> Left x

fizzbuzz :: [Int] -> [Either FB Int]
fizzbuzz = fmap veilOver

main :: IO ()
main = print (fmap (either show show) (fizzbuzz [1..20]))
