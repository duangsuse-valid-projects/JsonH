-- | Simple parser combinator
-- |
-- | copied from: https://ice1000.org/2017/07/26/HaskellParsers/
-- |
-- | Exposes
-- | Parser, satisfy, charItem, runParser, parse, chainl1, chainr1
module SimpleParserc (Parser, runParser, parse, charItem, satisfy, chainl1, chainr1) where

import Control.Monad
import Control.Applicative

-- | Parser monad
newtype Parser t = Parser { runParser :: String -> [(t, String)] }

-- | Run parser, if matches, return Just, otherwise Nothing
parse :: Parser a -> String -> Maybe a
parse p code = case runParser p code of
  [(st, [])] -> Just st
  _ -> Nothing


-- Typeclass instances
instance Functor Parser where
  fmap f (Parser ps) = Parser $ mapper
    where mapper p = [ (f a, b) | (a, b) <- ps p ]

--
instance Monad Parser where
  return a = Parser $ \s -> [(a, s)]
  p >>= f  = Parser $ (concatMap mapper) . runParser p
    where mapper (x, string') = f x `runParser` string'

instance Applicative Parser where
  pure = return
  (Parser p) <*> (Parser p') = Parser $ \string ->
    [(f x, string') | (f, rest) <- (p string), {- then -} (x, string') <- (p' rest) ]

-- | Parser alternative
-- | charP "a" <|> charP "b"
instance Alternative Parser where
  empty   = mzero
  p <|> q = Parser $ \string -> case runParser p string of
    [] -> runParser q string
    result -> result

-- | Roll parser p to parser q
instance MonadPlus Parser where
  mzero = Parser $ const []
  mplus p q = Parser $ \string ->
    runParser p string ++ runParser q string


-- Parser combinator interface
charItem :: Parser Char
charItem = Parser $ \string -> case string of
  [] -> []
  (char : chars) -> [(char, chars)]

-- | Given a predicate Char -> Bool
-- | Returning a parser parses a single character
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = charItem >>= branch
  where
    branch char
      | predicate char = return char
      | otherwise = empty

-- chainl1 and chainr1

-- | Read many ip, test result with op, if matches, chain (join with f <- op) next (left associative)
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 ip op = do
  l <- ip
  rest l
  where
    rest l = do
      o <- op  -- like '+'
      r <- ip  -- like '9'
      rest $ l `o` r -- p {o p}
      <|> return l

-- | Read many ip, test result with op, if matches, chain (join with f <- op) next (left associative)
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan   = p >>= rest
    rest l = do
      mid <- op -- like '^'
      r <- scan -- a@(p {op (a|p)})
      return $ l `mid` r
      <|> return l
