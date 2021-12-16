{-# LANGUAGE LambdaCase #-}
module Parsing (module Parsing, module Control.Applicative,
  module Text.ParserCombinators.Parsec) where

import           Control.Applicative(some,many)
import           Text.ParserCombinators.Parsec hiding(many)
import           Data.Char

sat :: (Char -> Bool) -> GenParser Char st Char
sat p = do
  x <- anyChar
  if p x then return x else fail "(not that char)"


pToken ::GenParser Char st a -> GenParser Char st a 
pToken p = do
  many space
  v <- p
  space
  return v


pNat :: GenParser Char st Int
pNat = do
  xs <- some digit
  return (read xs)


symbol xs = pToken (string xs)

