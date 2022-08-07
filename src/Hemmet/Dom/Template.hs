module Hemmet.Dom.Template where

import Data.Char
import Data.Text hiding (map)
import Data.Maybe (catMaybes, fromMaybe)

import Hemmet.Megaparsec
import Hemmet.Tree
import Text.Megaparsec.Char.Lexer (decimal)

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
template = Template <$> (Prelude.concat <$> many_ tag) <* eof



tag :: Parser [Tag]
tag = do
  -- Multiplicity can appear in between those parts, so we parse any possible
  -- position and then validate that no more than one multiplicity was parsed
  _tName <- try_ identifier
  multiplicity1 <- parseMultiplicity
  _tId <- try_ (Just <$> (char '#' *> kebabCasedName)) <|> pure Nothing
  multiplicity2 <- parseMultiplicity
  _tClasses <- many $ char '.' *> kebabCasedName
  multiplicity3 <- parseMultiplicity
  _tChilds <- Prelude.concat <$> try_ childs
  let multiplicities = [multiplicity1, multiplicity2, multiplicity3]
  if
    Prelude.length (Prelude.filter (/= Nothing) multiplicities) > 1
  then
    fail "Tag can has only one multiplicity"
  else
    let multiplicity = fromMaybe 1 (safeHead (catMaybes multiplicities)) in
    return $ Prelude.replicate multiplicity $ Tag {..}
  where
    childs = char '>' *> many_ tag
    parseMultiplicity =  optional (char '*' *> decimal)
    safeHead :: [a] -> Maybe a
    safeHead []    = Nothing
    safeHead (a:_) = Just a

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
