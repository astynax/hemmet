{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hemmet.Template
    ( Template(..)
    , Params(..)
    , Block(..)
    , Element(..)
    , Addon(..)
    , template
     -- reexports
    , parse
    ) where

import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Text hiding (map)
import Text.Parsec
import Text.Parsec.Text

import Hemmet.Tree

newtype Template =
    Template [Block]
    deriving (Show, Eq)

instance ToTree Template where
    toTree = toTree'

data Params = Params
    { _pTagName :: Text
    , _pName :: Text
    , _pAddons :: [Addon]
    , _pChilds :: [TemplateNode]
    } deriving (Show, Eq)

type TemplateNode = Either Element Block

newtype Block =
    Block Params
    deriving (Show, Eq)

newtype Element =
    Element Params
    deriving (Show, Eq)

data Addon
    = Mod Text
    | Mix Text
    | Var Text
    deriving (Show, Eq)

type CtorAndName a = (Params -> a, Text)

template :: Parser Template
template = Template <$> many_ alwaysBlock <* eof
  where
    alwaysBlock = templateNode $ ((,) Block) <$> blockName

templateNode :: Parser (CtorAndName a) -> Parser a
templateNode cnn = do
    _pTagName <- try_ identifier
    (ctor, _pName) <- cnn
    _pAddons <- many addon
    _pChilds <- try_ $ childs
    return $ ctor Params {..}
  where
    childs = char '>' *> many_ (templateNode blockOrElement)

blockOrElement :: Parser (CtorAndName TemplateNode)
blockOrElement = b <|> e
  where
    b = ((,) (Right . Block)) <$> blockName
    e = ((,) (Left . Element)) <$> elemName

blockName :: Parser Text
blockName = string ":" *> kebabCasedName

elemName :: Parser Text
elemName = string "." *> kebabCasedName

identifier :: Parser Text
identifier = cons <$> firstChar <*> (pack <$> many restChar)
  where
    firstChar = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'
    restChar = firstChar <|> digit

addon :: Parser Addon
addon = mod_ <|> mix <|> var
  where
    mod_ = Mod <$> (char '~' *> modName)
    mix = Mix <$> (char '^' *> kebabCasedName)
    var = Var <$> (char '$' *> identifier)

kebabCasedName :: Parser Text
kebabCasedName =
    cons <$> lascii <*> (pack <$> many (char '-' <|> lascii <|> digit))
  where
    lascii = satisfy isAsciiLower

modName :: Parser Text
modName = (<>) <$> kebabCasedName <*> possibleValue
  where
    possibleValue = try_ $ cons <$> char '_' <*> kebabCasedName

many_ :: Parser a -> Parser [a]
many_ p = ps <* eof <|> between (char '(') (char ')') ps
  where
    ps = p `sepBy` char '+'

try_
    :: Monoid m
    => Parser m -> Parser m
try_ = (<|> pure mempty)

-- transrormation to Tree
toTree' :: Template -> Tree
toTree' (Template bs) = map (transformBlock "") bs

transformBlock :: Text -> Block -> Node
transformBlock _ (Block p) = transform' (flip const) (_pName p) p

transformElement :: Text -> Element -> Node
transformElement parent (Element e) = transform' (prefix "__") parent e

transform' :: (Text -> Text -> Text) -> Text -> Params -> Node
transform' use parent (Params _nTagName name addons childs) = Node {..}
  where
    n = use parent name
    _nClasses =
        (n :) $
        flip mapMaybe addons $ \case
            Var _ -> Nothing
            Mix m -> Just m
            Mod m -> Just $ prefix "_" n m
    _nVars =
        flip mapMaybe addons $ \case
            Var v -> Just v
            _ -> Nothing
    _nChilds =
        map (either (transformElement parent) (transformBlock parent)) childs

prefix :: Text -> Text -> Text -> Text
prefix sep p n = p <> sep <> n
