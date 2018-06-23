module Main where

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Data.Map.Strict as M
import System.IO (isEOF)
import Control.Monad (unless)
import Data.Maybe (fromJust)

import qualified Kana as K

type Parser = P.Parsec String ()

data TextAST
  = Plain String
  | Hira [String]
  | Kata [String]
  deriving (Show)

acceptedWords :: [String]
acceptedWords = ["`", "'", "-"] ++ K.syllables

plain :: Parser String
plain = P.many1 $ P.satisfy (/= '$')

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
top = P.many $ P.choice [Plain <$> plain, Hira <$> hira, Kata <$> kata]

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
        _ -> fromJust $ M.lookup r lookupMap
    
main :: IO ()
main = loop
  where 
    loop = do
      done <- isEOF
      unless done $ do
        inp <- getLine
        case P.runParser top () "" inp of
          Left e -> putStrLn (show e)
          Right a -> putStrLn (pprint a)
        loop
