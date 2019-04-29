-- | Haskell Json Parser and encoder
-- |
-- | Json is a subset of JavaScript language
-- |
-- | See Json specification on
-- | https://json.org/
module JsonHaskell where

import Control.Applicative

import Data.Map
import Data.Maybe (fromJust)

import Data.Char (digitToInt)

-- Parser combinator
import SimpleParserc

-- Operators
charP :: Char -> Parser Char
charP x = satisfy $ (x ==)

elemP :: [Char] -> Parser Char
elemP s = satisfy . flip elem $ s

stringP :: String -> Parser String
stringP [] = return ""
stringP (x : xs) = do
  charP x
  stringP xs
  return $ x : xs

ws :: Parser String
ws = some $ elemP whiteSpaces
ws0 :: Parser String
ws0 = many $ elemP whiteSpaces

tokenP :: Parser String -> Parser (String, String)
tokenP kp = do
  tk <- kp
  wc <- ws
  pure $ (tk, wc)

kwP :: String -> Parser (String, String)
kwP = tokenP . stringP

mayDefault :: Parser a -> a -> Parser (Maybe a)
mayDefault p d = do
  r <- p <|> return d
  return $ Just r

may :: Parser a -> Parser (Maybe a)
may p = do
  rs <- p
  return . Just $ rs
  <|> return Nothing

-- | binary operator abstraction
binOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
binOp s = (stringWs0P s >>) . return
  where stringWs0P cs = ws0 >> stringP cs <* ws0

-- Definitions
whiteSpaces = "\t\n\r "
neg = "-"
signs = "+" ++ neg
exps = "Ee"
digitNoZero = "123456789"
digit = "0" ++ digitNoZero
hexs = digit ++ "ABCDEFabcdef"
escapes = "\"\\/bnrtu"

colon = ":"
comma = ","

type NumJ = Double

data ValueJ
  = ObjectJ (Map String ValueJ)
  | ArrayJ [ValueJ]
  | StringJ String
  | NumberJ NumJ
  | BoolJ Bool
  | NullJ
  deriving Eq

-- Json language
wsJ = ws
signJ = may $ elemP signs
expJ :: Parser NumJ
expJ = do
  elemP exps
  s <- signJ
  d <- digitsJ
  return $ case s of
    Just '+' -> d
    Just '-' -> negate d
    Nothing -> d

digitsJ :: Parser NumJ
digitsJ = do
  ds <- some $ elemP digit
  return $ read ds

intJ :: Parser NumJ
intJ = do
  sign <- may $ charP '-'
  h <- elemP digit
  t <- may digitsJ
  return $ case sign of
    Just _ -> negate $ scan t h
    Nothing -> scan t h
    where
      scan m h' =
        let n = fromIntegral . digitToInt $ h' in
          case m of
            Nothing -> n
            Just x -> x + n * 10 * (x / 10 + 1)

numJ :: Parser NumJ
numJ = do
  i <- intJ
  f <- may fracJ
  e <- mayDefault expJ 1
  return $ case f of
    Nothing -> i ** (fromJust e)
    Just x -> (i * x) ** (fromJust e)

fracJ :: Parser NumJ
fracJ = do
  charP '.'
  digitsJ >>= return

