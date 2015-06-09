fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fib' :: Integer -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = snd (traverseFib (0, 1, n - 1))
  where traverseFib (prev, res, 0) = (prev, res, 0)
        traverseFib (prev2, prev1, count) = traverseFib (prev1, prev1 + prev2, count - 1)
        snd (_, x, _) = x

fibs2 :: [Integer]
fibs2 = map fib' [0..]


--02.
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a (rest)) = [a] ++ streamToList rest

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

--04.
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Stream x rest) = Stream (fn x) (streamMap fn rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

--05.
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- ruler :: Stream Integer
