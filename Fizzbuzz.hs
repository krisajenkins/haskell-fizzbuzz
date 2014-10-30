import Control.Applicative

joinMaybe :: Maybe a -> Maybe a -> Maybe [a]
joinMaybe (Just x) (Just y) = Just [x, y]
joinMaybe (Just x) Nothing = Just [x]
joinMaybe Nothing (Just y) = Just [y]
joinMaybe Nothing Nothing = Nothing

maybeToEither :: Maybe a -> b -> Either a b
maybeToEither (Just x) _ = Left x
maybeToEither Nothing y = Right y

data FB = Fizz | Buzz | Fizzbuzz deriving (Show,Eq)

fizzes :: [Maybe FB]
fizzes = cycle [Nothing, Nothing, Just Fizz]

buzzes :: [Maybe FB]
buzzes = cycle [Nothing, Nothing, Nothing, Nothing, Just Buzz]

fizzbuzzes :: [Maybe [FB]]
fizzbuzzes = zipWith joinMaybe fizzes buzzes

ints :: [Int]
ints = [1..]

fizzbuzz :: [Either [FB] Int]
fizzbuzz = zipWith maybeToEither fizzbuzzes ints

main :: IO ()
main = print $ either show show <$> take 30 fizzbuzz
