{-# LANGUAGE FlexibleInstances #-}

module Hemmet.Dom.Template where

import Control.Monad (void)
import Data.Char
import Data.Text as T hiding (map)

import Text.Megaparsec.Char.Lexer (decimal)

import Hemmet.Megaparsec
import Hemmet.Tree
import Hemmet.Zipper

import Hemmet.Dom.Tree hiding (Void, Children)
import qualified Hemmet.Dom.Tree as Tree

newtype Template =
  Template [Html]
  deriving (Show, Eq)

instance ToTree Template DomPayload where
  toTree = toTree'

data Tag =
  Tag
  { _tName     :: !Text
  , _tId       :: !(Maybe Text)
  , _tClasses  :: ![Text]
  , _tChildren :: !Children
  } deriving (Show, Eq)

data Children
  = Void
  | Children ![Html]
  deriving (Show, Eq)

type Html = Repeating Tag

data Repeating a
  = Single !a
  | Times !Int !a
  deriving (Show, Eq)

instance Functor Repeating where
  fmap f (Single t) = Single $ f t
  fmap f (Times n t) = Times n $ f t

parse :: Text -> Either (ParseErrorBundle Text ZippingError) Template
parse = fmap Template . parseWith htmlParsing

tag :: SParser Html Html
tag = do
  _tName <- try_ identifier
  _tId <- try_ (Just <$> (char '#' *> kebabCasedName)) <|> pure Nothing
  _tClasses <- many $ char '.' *> kebabCasedName
  isVoid <- True <$ char '/' <|> pure False
  repeatOrNot <- Times <$> (char '*' *> decimal) <|> pure Single
  _tChildren <-
    if isVoid then pure Void
    else pure (Children [])
  return . repeatOrNot $ Tag {..}

identifier :: SParser a Text
identifier = T.cons <$> firstChar <*> (pack <$> many restChar)
  where
    firstChar = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'
    restChar = firstChar <|> digitChar

kebabCasedName :: SParser a Text
kebabCasedName =
  T.cons <$> lascii <*> (pack <$> many (char '-' <|> lascii <|> digitChar))
  where
    lascii = satisfy isAsciiLower

try_ :: Monoid m => SParser a m -> SParser a m
try_ = (<|> pure mempty)

htmlParsing :: StepParsing Html
htmlParsing = StepParsing
  { downP         = void $ char '>'
  , upP           = void $ char '^'
  , nextP         = void $ char '+'
  , leftBracketP  = void $ char '('
  , rightBracketP = void $ char ')'
  , chunkP        = tag
  }

instance Buildable Html where
  root = Single Tag
    { _tName = "root"
    , _tClasses = []
    , _tId = Nothing
    , _tChildren = Children []
    }
  cons (Single t)  cs = Single t  { _tChildren = Children cs }
  cons (Times n t) cs = Times n t { _tChildren = Children cs }

-- transrormation to Tree

toTree' :: Template -> Tree DomPayload
toTree' (Template bs) =
  DomPayload Nothing [] . Tree.Children $ Prelude.concatMap fromHtml bs

fromHtml :: Html -> [Node DomPayload]
fromHtml (Single (Tag n i cls cs)) =
  [ Node n $ DomPayload i cls $ case cs of
       Void       -> Tree.Void
       Children x -> Tree.Children $ Prelude.concatMap fromHtml x
  ]
fromHtml (Times n t) =
  -- Yep, here we still have some replication.
  -- Need to think how to postpone this replication until the rendering.
  Prelude.concatMap fromHtml $ Prelude.replicate n $ Single t
