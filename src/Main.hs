module Main where

import           Control.Monad    (unless)
import qualified Data.Map.Strict  as M
import           Data.Maybe       (fromJust)
import           System.IO        (isEOF)
import qualified Text.Parsec      as P
import qualified Text.Parsec.Char as P

import qualified Kana             as K

type Parser = P.Parsec String ()

data TextAST
  = Plain String
  | Hira [String]
  | Kata [String]
  deriving (Show)

acceptedWords :: [String]
acceptedWords = ["`", "'", "-"] ++ K.syllables

plain :: Parser String
plain = do
  x <- P.anyChar
  xs <- P.many $ P.satisfy (/= '$')
  return (x:xs)

word :: Parser String
word = P.choice $ map (P.try . P.string) acceptedWords

hira :: Parser [String]
hira = do
  P.try (P.string "$hira")
  P.between (P.char '(') (P.char ')') (P.many1 word)

kata :: Parser [String]
kata = do
  P.try (P.string "$kata")
  P.between (P.char '(') (P.char ')') (P.many1 word)

top :: Parser [TextAST]
top = do
  xs <- P.many $ P.choice [Hira <$> hira, Kata <$> kata, Plain <$> plain]
  P.eof
  return xs

pprint :: [TextAST] -> String
pprint xs = concatMap pprintHelper xs
  where
    pprintHelper (Plain p) = p
    pprintHelper (Hira rs) = concatMap (pprintRomaji K.hiraMap) rs
    pprintHelper (Kata rs) = concatMap (pprintRomaji K.kataMap) rs

    pprintRomaji lookupMap r =
      case r of
        "`" -> "「"
        "'" -> "」"
        "-" -> "ー"
        _   -> fromJust $ M.lookup r lookupMap

main :: IO ()
main = loop
  where
    loop = do
      done <- isEOF
      unless done $ do
        inp <- getLine
        case P.runParser top () "" inp of
          Left e  -> putStrLn (show e)
          Right a -> do
            putStrLn (pprint a)
            loop
