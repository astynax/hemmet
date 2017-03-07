{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Node(..)
    , Template(..)
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
    , _pChilds :: [Either Element Block]
    } deriving (Show, Eq)

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
template = Template <$> many_ (part (always blockName) $ const Block) <* eof
  where
    always = fmap ((,) True)

part :: Parser (Bool, String) -> (Bool -> Params -> a) -> Parser a
part nameParser wrap = do
    _pTagName <- try_ identifier
    (isBlock, _pName) <- nameParser
    _pAddons <- many addon
    _pChilds <- try_ $ char '>' *> many_ (part both wrap')
    return $ wrap isBlock Params {..}
  where
    both = ((,) True) <$> blockName <|> ((,) False) <$> elemName
    wrap' True = Right . Block
    wrap' False = Left . Element

identifier :: Parser String
identifier =
    (:) <$> (letter' <|> char '_') <*> many (char '_' <|> digit <|> letter')
  where
    letter' = satisfy isAsciiLower <|> satisfy isAsciiUpper

blockName :: Parser String
blockName = string ":" *> kebabCasedName

elemName :: Parser String
elemName = string "." *> kebabCasedName

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
modName =
    (++) <$> kebabCasedName <*> (try_ $ (:) <$> char '_' <*> kebabCasedName)

many_ :: Parser a -> Parser [a]
many_ p = ps <* eof <|> between (char '(') (char ')') ps
  where
    ps = p `sepBy` char '+'

try_ :: Parser [a] -> Parser [a]
try_ = (<|> pure [])
