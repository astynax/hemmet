module Hemmet.FileTree.Template
    ( template
    ) where

import Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import Hemmet.Tree

import Hemmet.FileTree.Tree

template :: Parser FileTree
template = (Directory <$> item `sepBy` char ' ') <* eof

item :: Parser (Node FileTreePayload)
item = name >>= \n -> (directory n <|> file n)
  where
    file n = pure $ Node n File

directory :: Text -> Parser (Node FileTreePayload)
directory n = char '/' *> (Node n . Directory <$> childs)
  where
    childs = many_ item

name :: Parser Text
name = T.pack <$> many1 (satisfy $ not . isSpecial)

many_ :: Parser a -> Parser [a]
many_ p = ps <|> single p <|> none
  where
    single = fmap (: [])
    ps = between (char '{') (char '}') (p `sepBy` char ' ')
    none = [] <$ notFollowedBy name

isSpecial :: Char -> Bool
isSpecial ' ' = True
isSpecial '/' = True
isSpecial '}' = True
isSpecial _ = False
