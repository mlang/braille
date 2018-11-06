{-# LANGUAGE FlexibleInstances, GADTs, RecordWildCards, TypeSynonymInstances, ViewPatterns #-}

module BRLTTY.TextTable (TextTable(..), Dots, load, translate) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Bits
import           Data.Char (chr, isSpace, ord)
import           Data.Functor (($>))
import           Data.Ix (inRange)
import qualified Data.Set as Set (empty, singleton)
import           Data.Word (Word8)
import           Numeric (readHex, readOct)
import           System.FilePath.Posix
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Error

type Dots = Word8
data TextTable = TextTable {
  chars :: [(Char, Dots)]
, glyphs :: [(Char, Dots)]
, aliases :: [(Char, Char)]
} deriving (Eq, Show)
instance Semigroup TextTable where
  TextTable a b c <> TextTable d e f = TextTable (a <> d) (b <> e) (c <> f)
instance Monoid TextTable where
  mempty = TextTable [] [] []

load :: FilePath -> IO (Either String TextTable)
load fp = do
  c <- readFile fp
  (e, r) <- run ttb fp c mempty
  pure . bimap parseErrorPretty (const r) $ e

translate :: Char -> TextTable -> Maybe Dots
translate c@(ord -> uc) TextTable {..}
  | inRange (0x2800, 0X28ff) uc = Just . fromIntegral $ uc - 0x2800
  | Just d <- lookup c chars    = Just d
  | Just d <- lookup c glyphs   = Just d
  | Just c' <- lookup c aliases
  , Just d <- lookup c' chars   = Just d
  | otherwise                   = Nothing

type Parser = ReaderT FilePath (ParsecT String String (StateT TextTable IO))
run :: Parser a -> FilePath -> String -> TextTable
    -> IO (Either (ParseError Char String) a, TextTable)
run p fp c = runStateT $ runParserT (runReaderT p $ takeDirectory fp) fp c

ttb :: Parser ()
ttb = many (comment <|> void eol <|> aliasD <|> charD <|> glyphD <|> include) *> eof

aliasD, charD, glyphD, include :: Parser ()
aliasD = do
  ws
  string "alias"
  ws1
  from <- character
  ws1
  to <- character
  modify $ \tt -> tt { aliases = aliases tt <> [(from, to)] }
charD = do
  ws
  string "char"
  ws1
  c <- character
  ws1
  d <- dots
  modify $ \tt -> tt { chars = chars tt <> [(c, d)] }
glyphD = do
  ws
  string "glyph"
  ws1
  c <- character
  ws1
  d <- dots
  modify $ \tt -> tt { glyphs = glyphs tt <> [(c, d)] }
include = do
  ws
  string "include"
  ws1
  fn <- some character
  dir <- ask
  let fp = dir </> fn
  c <- liftIO . readFile $ fp
  (e, r) <- get >>= liftIO . run ttb fp c
  either (fancyFailure . Set.singleton . ErrorCustom . parseErrorPretty)
         (const $ put r)
         e

character :: (MonadParsec e s m, Token s ~ Char)
          => m Char
character = char '\\' *> escaped <|> satisfy (not . isSpace) <?> "character" where
  escaped = char 'b' $> '\b'
        <|> char 'f' $> '\f'
        <|> char 'n' $> '\n'
        <|> char 'o' *> oct 3
        <|> char 'r' $> '\r'
        <|> char 's' $> ' '
        <|> char 't' $> '\t'
        <|> char 'u' *> hex 4
        <|> char 'U' *> hex 8
        <|> char 'v' $> '\v'
        <|> char 'x' *> hex 2
        <|> char 'X' *> hex 2
        <|> char '#' $> '#'
        <|> char '\\' $> '\\'
  hex c = chr . fst . head . readHex <$> count c hexDigitChar
  oct c = chr . fst . head . readOct <$> count c octDigitChar

dots :: (MonadParsec e s m, Token s ~ Char, Bits dots, Num dots)
     => m dots
dots = char '0' $> 0
   <|> foldr (.|.) 0 <$>
       (  some dot
      <|> between (char '(') (char ')') (ws *> many (ws *> dot <* ws) <* ws)
       )
  where dot = (\c -> bit $ ord c - ord '1') <$> oneOf "12345678"

comment = void $ ws >> char '#' >> takeWhileP (Just "comment") (`notElem` "\n\r")

ws, ws1 :: (MonadParsec e s m, Token s ~ Char)
        => m ()
ws = void $ takeWhileP (Just "white space") (`elem` " \t")
ws1 = void $ takeWhile1P (Just "white space") (`elem` " \t")

instance ShowErrorComponent String where showErrorComponent = id
