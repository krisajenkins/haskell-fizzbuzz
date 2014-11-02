import Control.Applicative

joinMaybe :: Maybe a -> Maybe a -> Maybe [a]
joinMaybe (Just x) (Just y) = Just [x, y]
joinMaybe (Just x) Nothing = Just [x]
joinMaybe Nothing (Just y) = Just [y]
joinMaybe Nothing Nothing = Nothing

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

data FB = Fizz | Buzz | Fizzbuzz deriving (Show,Eq)

fizzes :: [Maybe FB]
fizzes = cycle [Just Fizz, Nothing, Nothing]

buzzes :: [Maybe FB]
buzzes = cycle [Just Buzz, Nothing, Nothing, Nothing, Nothing]

fizzbuzzes :: [Maybe [FB]]
fizzbuzzes = zipWith joinMaybe fizzes buzzes

ints :: [Int]
ints = [0..]

fizzbuzz :: [Either Int [FB]]
fizzbuzz = zipWith maybeToEither ints fizzbuzzes

main :: IO ()
main = print $ either show show <$> take 30 fizzbuzz
