{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char (toLower)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)


score :: Char -> Score
score y
  | elem x "aeilnorstu" = Score 1
  | elem x "dg" = Score 2
  | elem x "bcmp" = Score 3
  | elem x "fhvwy" = Score 4
  | elem x "k" = Score 5
  | elem x "jx" = Score 8
  | elem x "qz" = Score 10
  | otherwise = Score 0
  where x = toLower y

scoreString :: String -> Score
scoreString = foldr (mappend . score) mempty
