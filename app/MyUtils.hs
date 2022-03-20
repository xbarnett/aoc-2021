module MyUtils where

import Data.List
import Data.Hashable
import Data.HashMap.Strict
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

freqs :: (Eq a, Hashable a) => [a] -> HashMap a Int
freqs [] = empty
freqs (x : xs) = insertWith f x 1 (freqs xs)
  where
    f new old = old + 1

mode :: (Eq a, Hashable a) => [a] -> a
mode xs = fst (foldrWithKey update_mode (head xs, 0) (freqs xs))
 where
   update_mode :: a -> Int -> (a, Int) -> (a, Int)
   update_mode k v (mk, mv) = if mv > v then (mk, mv) else (k, v)

unmode :: (Eq a, Hashable a) => [a] -> a
unmode xs = fst (foldrWithKey update_mode (head xs, length xs + 1) (freqs xs))
 where
   update_mode :: a -> Int -> (a, Int) -> (a, Int)
   update_mode k v (mk, mv) = if mv < v then (mk, mv) else (k, v)

parse_int :: Parser Int
parse_int = do
  neg <- optionMaybe (char '-')
  let sign = case neg of
        Just _ -> (-1)
        Nothing -> 1
  digits <- many1 digit
  return (sign * read digits)
