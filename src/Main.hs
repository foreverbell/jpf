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

plain :: Parser String
plain = do
  x <- P.anyChar
  xs <- P.many $ P.satisfy (/= '$')
  return (x:xs)

word :: Parser String
word = P.choice (normals ++ sokuons ++ miscs)
  where
    normals = map (P.try . P.string) K.syllables
    sokuons = (flip map) K.consonants $ \c -> P.try $ do
      P.char c
      P.lookAhead (P.char c)
      return "."
    miscs = map P.string ["`", "'", "-"]

hira :: Parser [String]
hira = do
  P.try (P.string "$hh")
  P.between (P.char '(') (P.char ')') (P.many1 word)

kata :: Parser [String]
kata = do
  P.try (P.string "$kk")
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
    pprintHelper (Hira rs) = concatMap (pprintRomaji K.hiraMap K.hiraSokuon) rs
    pprintHelper (Kata rs) = concatMap (pprintRomaji K.kataMap K.kataSokuon) rs

    pprintRomaji lookupMap sokuon r =
      case r of
        "`" -> "「"
        "'" -> "」"
        "-" -> "ー"
        "." -> [sokuon]
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
