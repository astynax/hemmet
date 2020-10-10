module Hemmet.Dom.Template where

import Data.Char
import Data.Text hiding (map)

import Hemmet.Megaparsec
import Hemmet.Tree

import Hemmet.Dom.Tree

newtype Template =
  Template [Tag]
  deriving (Show, Eq)

instance ToTree Template DomPayload where
  toTree = toTree'

data Tag =
  Tag
  { _tName    :: !Text
  , _tId      :: !(Maybe Text)
  , _tClasses :: ![Text]
  , _tChilds  :: [Tag]
  } deriving (Show, Eq)

template :: Parser Template
template = Template <$> many_ tag <* eof

tag :: Parser Tag
tag = do
  _tName <- try_ identifier
  _tId <- try_ (Just <$> (char '#' *> kebabCasedName)) <|> pure Nothing
  _tClasses <- many $ char '.' *> kebabCasedName
  _tChilds <- try_ childs
  return Tag {..}
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
toTree' (Template bs) = DomPayload Nothing [] $ map fromTag bs

fromTag :: Tag -> Node DomPayload
fromTag (Tag n i cls cs) = Node n $ DomPayload i cls $ map fromTag cs
