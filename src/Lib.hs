{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Node(..)
    , Template(..)
    , Params(..)
    , Block(..)
    , Element(..)
    , Mix(..)
    , Modifier(..)
    , template
    , transform
      -- reexports
    , parse
    ) where

import Data.Char
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

newtype Template =
    Template [Block]
    deriving (Show, Eq)

data Params = Params
    { _tagName :: String
    , _name :: String
    , _mods :: [Modifier]
    , _mixes :: [Mix]
    , _childs :: [Either Element Block]
    } deriving (Show, Eq)

newtype Block = Block
    { unBlock :: Params
    } deriving (Show, Eq)

newtype Element = Element
    { unElement :: Params
    } deriving (Show, Eq)

newtype Mix = Mix
    { unMix :: String
    } deriving (Show, Eq)

newtype Modifier = Modifier
    { unModifier :: String
    } deriving (Show, Eq)

data Node =
    Node String
         [String]
         [Node]
    deriving (Show, Eq)

-- transformation
transform :: Template -> [Node]
transform (Template bs) = map (transformBlock "") bs

transformBlock :: String -> Block -> Node
transformBlock _ (Block p) = transform' (flip const) (_name p) p

transformElement :: String -> Element -> Node
transformElement parent = transform' (prefix "__") parent . unElement

transform' use parent (Params tagName name mods mixes childs) =
    let n = use parent name
    in Node
           tagName
           (n : map (prefix "_" n . unModifier) mods ++ map unMix mixes) $
       map (either (transformElement parent) (transformBlock parent)) childs

prefix sep p n = p ++ sep ++ n

-- template parsing
template :: Parser Template
template = Template <$> many_ (part (always blockName) $ const Block) <* eof
  where
    always = fmap ((,) True)

part :: Parser (Bool, String) -> (Bool -> Params -> a) -> Parser a
part nameParser wrap = do
    _tagName <- try_ identifier
    (isBlock, _name) <- nameParser
    _mods <- many $ Modifier <$> (char '~' *> modName)
    _mixes <- many $ Mix <$> (char '^' *> kebabCasedName)
    _childs <- try_ $ char '>' *> many_ (part both wrap')
    return $ wrap isBlock Params {..}
  where
    both = ((,) True) <$> blockName <|> ((,) False) <$> elemName
    wrap' True = Right . Block
    wrap' False = Left . Element

identifier :: Parser String
identifier = (:) <$> letter' <*> many (char '_' <|> digit <|> letter')
  where
    letter' = satisfy isAsciiLower <|> satisfy isAsciiUpper

blockName :: Parser String
blockName = string ":" *> kebabCasedName

elemName :: Parser String
elemName = string "." *> kebabCasedName

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

try_ = (<|> pure [])
