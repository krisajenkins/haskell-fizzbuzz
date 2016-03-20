module Fizzbuzz where

data FB
  = Fizz
  | Buzz
  | Fizzbuzz
  deriving (Show,Eq)

fizzbuzz :: Int -> Either FB Int
fizzbuzz n =
  case (mod n 3,mod n 5) of
    (0,0) -> Left Fizzbuzz
    (0,_) -> Left Fizz
    (_,0) -> Left Buzz
    _ -> Right n

showEither :: (Show a,Show b)
           => Either a b -> String
showEither = either show show

main :: IO ()
main = print $ fmap (showEither . fizzbuzz) [1..20]
