{-# LANGUAGE LambdaCase #-}
-- Based on Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing (module Parsing, module Control.Applicative) where

-- import           Control.Applicative
-- import           Data.Char

import Control.Applicative
    ( (<$),
      Applicative(..),
      Alternative(..),
      optional,
      (<$>),
      (<**>),
      liftA,
      liftA3,
      WrappedArrow(..),
      WrappedMonad(..),
      ZipList(..),
      Const(..) )
import Data.Char
    ( isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper )

-- Basic definitions
newtype Parser a = P (String -> Maybe (a,String))

parse :: Parser a -> String -> Maybe (a,String)
parse (P p) = p


item :: Parser Char
item = P (\case
             []    -> Nothing
             (x:xs) -> Just (x,xs))

-- Sequencing parsers
instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            Nothing -> Nothing
                            Just (v,out) -> Just (g v, out))

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> Just (v,inp))

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             Nothing -> Nothing
                             Just (g,out) -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           Nothing -> Nothing
                           Just (v,out) ->  parse (f v) out)

-- Making choices
instance Alternative Parser where
   -- empty :: Parser a
   empty = P (const Nothing)

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           Nothing       -> parse q inp
                           Just (v,out) -> Just (v,out))

-- Derived primitives
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing
space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

oneOf :: String -> Parser Char
oneOf str = P(\case
             [] -> Nothing
             (c:cs) | elem c str -> Just (c,cs)
                    | otherwise -> Nothing)
