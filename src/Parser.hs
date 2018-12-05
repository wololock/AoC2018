module Parser where

-- From "Programming in Haskell, 2nd edition" by Graham Hutton

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) str = p str

runParser :: Parser a -> String -> a
runParser p str = case parse p str of
                    [(v,_)]  -> v
                    [(_,vs)] -> error "Parser didn't parse whole string."
                    _        -> error "Parser error."


-- Succeed with the first character
item :: Parser Char
item = P (\str -> case str of 
            []     -> []
            (x:xs) -> [(x,xs)])

-- parse item ""
-- []

-- parse item "abc"
-- [('a', "bc")]

parserFmap :: (a -> b) -> Parser a -> Parser b
parserFmap g p = P (\str -> case parse p str of
                     []         -> []
                     [(v,out)]  -> [(g v, out)])

instance Functor Parser where
    fmap = parserFmap

-- parse (fmap toUpper item) "abc"
-- [('A',"bc")]

-- parse (fmap toUpper item) ""
-- []


instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\str -> [(v,str)])
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\str -> case parse pg str of
                            []        -> []
                            [(g,out)] -> parse (fmap g px) out)

-- parse (pure 1) "abc"
-- [(1,"abc")]    

three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)

-- parse three ""
-- []

-- parse three "abc"
-- [(('a','c'),"")]

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\str -> case parse p str of
                            []        -> []
                            [(v,out)] -> parse (f v) out)


instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\str -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\str -> case parse p str of
                            []        -> parse q str
                            [(v,out)] -> [(v,out)])


-- 13.6 Derived primitives

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
string (x:xs) = do char x
                   string xs
                   return (x:xs)


ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace) 
           return ()

int :: Parser Int
int = do char '-' 
         n <- nat
         return (-n)
    <|> nat
    

token :: Parser a -> Parser a
token p = do space 
             v <- p
             space 
             return v

-- Using token , we can now define parsers that ignore spacing around identifiers, natural
-- numbers, integers and special symbols:

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

takeUntil :: (Char -> Bool) -> Parser String
takeUntil p = do xs <- many (sat (not . p))
                 return xs 