import Data.Maybe

data Intview = Fizz | Buzz | Fizzbuzz | Intview Int deriving (Eq)

instance Show Intview where
  show (Intview x) = show x
  show Fizz = "Fizz"
  show Buzz = "Buzz"
  show Fizzbuzz = "Fizzbuzz"

matcher :: Int -> Int -> Maybe Intview
matcher 0 0 = Just Fizzbuzz
matcher 0 _ = Just Fizz
matcher _ 0 = Just Buzz
matcher _ _ = Nothing

translate :: Int -> Intview
translate n = fromMaybe (Intview n) (matcher (mod n 3) (mod n 5))

fizzbuzz :: [Int] -> [Intview]
fizzbuzz = fmap translate

main :: IO ()
main = print $ fizzbuzz [1..20]
