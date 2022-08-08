{-# LANGUAGE BlockArguments #-}

module Hemmet.Dom.Template where

import Data.Char
import Data.Text hiding (map)
import Data.Maybe (isJust)

import Hemmet.Megaparsec
import Hemmet.Tree
import Text.Megaparsec.Char.Lexer (decimal)

import Hemmet.Dom.Tree

newtype Template =
  Template [Element]
  deriving (Show, Eq)

instance ToTree Template DomPayload where
  toTree = toTree'

data Element =
  Tag
  { _tName    :: !Text
  , _tId      :: !(Maybe Text)
  , _tClasses :: ![Text]
  , _tChilds  :: [Element]
  }
  | PlainText !Text
  deriving (Show, Eq)


template :: Parser Template
template = Template <$> (Prelude.concat <$> many_ element) <* eof

element :: Parser [Element]
element = try tag <|> plainText

tag :: Parser [Element]
tag = do
  -- Order of attributes to parse is fixed, not arbitrary, like in Emmet.
  -- This is design decision.
  _tName <- try_ identifier
  _tId <- try_ (Just <$> (char '#' *> kebabCasedName)) <|> pure Nothing
  _tClasses <- many $ char '.' *> kebabCasedName
  multiplicity <- char '*' *> decimal <|> pure 1
  text <- optional curlyBraces
  childs <- Prelude.concat <$> try_ childsParser
  -- Text in curly braces is interpreted as the first child (as in Emmet)
  let _tChilds = case text of
                  Just t -> PlainText t:childs
                  Nothing -> childs
  let notEmpty = not (Data.Text.null _tName)
                 || isJust _tId
                 || not (Prelude.null _tClasses)
  if notEmpty
    then return $ Prelude.replicate multiplicity $ Tag {..}
    else fail "Tag is empty!"
  where
    childsParser = char '>' *> many_ element

plainText :: Parser [Element]
plainText = (:[]) . PlainText <$> curlyBraces

curlyBraces :: Parser Text
curlyBraces = textBetween '{' '}'
  where
    textBetween a b = between (char a) (char b) (takeWhileP Nothing (/= b))

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
toTree' (Template bs) = DomTag Nothing [] $ map fromElement bs

fromElement :: Element -> Node DomPayload
fromElement (Tag n i cls cs) = Node n $ DomTag i cls $ map fromElement cs
fromElement (PlainText text) = Node "" $ DomPlainText text