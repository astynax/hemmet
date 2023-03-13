module Hemmet.Dom.Template where

import Data.Char
import Data.Text hiding (map)

import Hemmet.Megaparsec
import Hemmet.Tree
import Text.Megaparsec.Char.Lexer (decimal)

import Hemmet.Dom.Tree

newtype Template =
  Template [Html]
  deriving (Show, Eq)

instance ToTree Template DomPayload where
  toTree = toTree'

data Tag =
  Tag
  { _tName    :: !Text
  , _tId      :: !(Maybe Text)
  , _tClasses :: ![Text]
  , _tChilds  :: [Html]
  } deriving (Show, Eq)

data Html
  = Single !Tag
  | Times !Int !Tag
  deriving (Show, Eq)

template :: Parser Template
template = Template <$> many_ tag <* eof

tag :: Parser Html
tag = do
  -- Order of attributes to parse is fixed, not arbitrary, like in Emmet.
  -- This is design decision.
  _tName <- try_ identifier
  _tId <- try_ (Just <$> (char '#' *> kebabCasedName)) <|> pure Nothing
  _tClasses <- many $ char '.' *> kebabCasedName
  repeatOrNot <- Times <$> (char '*' *> decimal) <|> pure Single
  _tChilds <- try_ childs
  return . repeatOrNot $ Tag {..}
  where
    childs = char '>' *> many_ tag

identifier :: Parser Text
identifier = cons <$> firstChar <*> (pack <$> many restChar)
  where
    firstChar = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'
    restChar = firstChar <|> digitChar

kebabCasedName :: Parser Text
kebabCasedName =
  cons <$> lascii <*> (pack <$> many (char '-' <|> lascii <|> digitChar))
  where
    lascii = satisfy isAsciiLower

many_ :: Parser a -> Parser [a]
many_ p = between (char '(') (char ')') ps <|> ps
  where
    ps = p `sepBy` char '+'

try_ :: Monoid m => Parser m -> Parser m
try_ = (<|> pure mempty)

-- transrormation to Tree
toTree' :: Template -> Tree DomPayload
toTree' (Template bs) = DomPayload Nothing [] $ Prelude.concatMap fromHtml bs

fromHtml :: Html -> [Node DomPayload]
fromHtml (Single (Tag n i cls cs)) =
  [Node n $ DomPayload i cls $ Prelude.concatMap fromHtml cs]
fromHtml (Times n t) =
  -- Yep, here we still have some replication.
  -- Need to think how to postpone this replication until the rendering.
  Prelude.concatMap fromHtml $ Prelude.replicate n $ Single t
