module JoinList where

import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single annotation _) = annotation
tag (Append annotation _ _) = annotation


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ x)
  | i < 0 = Nothing
  | i == 0 = Just x
indexJ i (Append _ jl1 jl2)
  | Size i <= size (tag jl1) = indexJ i jl1
  | Size i > size (tag jl1) = indexJ (getSize (size (Size i - size (tag jl1)))) jl2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ i (Append _ jl1 jl2)
  | Size i <= size (tag jl1) = dropJ i jl1
  | Size i > size  (tag jl1) = dropJ (getSize (Size i - size (tag jl2))) jl2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ i list@(Single _ _) = list
takeJ i list@(Append a jl1 jl2)
  | Size i >= size a = list
  | Size i < size (tag jl1) = takeJ i jl1
  | Size i > size (tag jl1) = jl1 +++ takeJ (getSize (Size i - size (tag jl1))) jl2

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

--Helpers
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
(_:_) !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i - 1)


jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ x) = [x]
jlToList (Append _ jl1 jl2) = jlToList jl1 ++ jlToList jl2
