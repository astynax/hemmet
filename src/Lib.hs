{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Node(..)
    , Template(..)
    , TemplateNode
    , Params(..)
    , Block(..)
    , Element(..)
    , Addon(..)
    , template
    , transform
      -- reexports
    , parse
    ) where

import Data.Char
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

newtype Template =
    Template [Block]
    deriving (Show, Eq)

data Params = Params
    { _pTagName :: String
    , _pName :: String
    , _pAddons :: [Addon]
    , _pChilds :: [TemplateNode]
    } deriving (Show, Eq)

type TemplateNode = Either Element Block

newtype Block = Block
    { unBlock :: Params
    } deriving (Show, Eq)

newtype Element = Element
    { unElement :: Params
    } deriving (Show, Eq)

data Addon
    = Mod String
    | Mix String
    | Var String
    deriving (Show, Eq)

data Node = Node
    { _nTagName :: String
    , _nClasses :: [String]
    , _nVars :: [String]
    , _nChilds :: [Node]
    } deriving (Show, Eq)

type CtorAndName a = (Params -> a, String)

-- transformation
transform :: Template -> [Node]
transform (Template bs) = map (transformBlock "") bs

transformBlock :: String -> Block -> Node
transformBlock _ (Block p) = transform' (flip const) (_pName p) p

transformElement :: String -> Element -> Node
transformElement parent = transform' (prefix "__") parent . unElement

transform' :: (String -> String -> String) -> String -> Params -> Node
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

prefix :: String -> String -> String -> String
prefix sep p n = p ++ sep ++ n

-- template parsing
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

blockName :: Parser String
blockName = string ":" *> kebabCasedName

elemName :: Parser String
elemName = string "." *> kebabCasedName

identifier :: Parser String
identifier = (:) <$> firstChar <*> many restChar
  where
    firstChar = satisfy isAsciiLower <|> satisfy isAsciiUpper <|> char '_'
    restChar = firstChar <|> digit

addon :: Parser Addon
addon = mod_ <|> mix <|> var
  where
    mod_ = Mod <$> (char '~' *> modName)
    mix = Mix <$> (char '^' *> kebabCasedName)
    var = Var <$> (char '$' *> identifier)

kebabCasedName :: Parser String
kebabCasedName = (:) <$> lascii <*> many (char '-' <|> lascii <|> digit)
  where
    lascii = satisfy isAsciiLower

modName :: Parser String
modName = (++) <$> kebabCasedName <*> possibleValue
  where
    possibleValue = try_ $ (:) <$> char '_' <*> kebabCasedName

many_ :: Parser a -> Parser [a]
many_ p = ps <* eof <|> between (char '(') (char ')') ps
  where
    ps = p `sepBy` char '+'

try_ :: Parser [a] -> Parser [a]
try_ = (<|> pure [])
