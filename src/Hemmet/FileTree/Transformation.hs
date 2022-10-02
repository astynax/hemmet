module Hemmet.FileTree.Transformation
   ( haskellify
   , pythonify
   ) where

import Data.Char
import Data.Text as T hiding (concatMap, elem, map)

import Hemmet.Tree

import Hemmet.FileTree.Tree

haskellify :: Transformation FileTreePayload
haskellify File = File
haskellify (Directory nodes) = Directory $ concatMap process nodes
  where
    process (Node name File) = [Node (haskellifyFile name) File]
    process (Node name@(T.uncons -> Just (flag, rest)) d@(Directory ns)) =
      case flag of
        '*' -> Node (haskellifyFile rest) File : process (Node rest d)
        '!' -> [Node rest children]
        _   -> [Node (camelCase name) children]
      where
        children = Directory $ concatMap process ns
    process _ = error "Impossible!"
    haskellifyFile = modifyFileWith [] ((<> ".hs") . camelCase)
    camelCase = T.concat . map T.toTitle . split (== '-')

pythonify :: Transformation FileTreePayload
pythonify File = File
pythonify (Directory nodes) = Directory $ concatMap process nodes
  where
    process (Node name File) = [Node (pythonifyFile name) File]
    process (Node name@(T.uncons -> Just (flag, rest)) (Directory ns)) =
      case flag of
        '*' -> [Node (snakeCase rest) . Directory
          $ Node "__init__.py" File : concatMap process ns]
        '!' -> [Node rest children]
        _   -> [Node (snakeCase name) children]
      where
        children = Directory $ concatMap process ns
    process _ = error "Impossible!"
    pythonifyFile = modifyFileWith ['_'] ((<> ".py") . snakeCase)
    snakeCase = T.intercalate "_" . split (== '-')

modifyFileWith :: [Char] -> (Text -> Text) -> Text -> Text
modifyFileWith cs f name@(T.uncons -> Just (first, rest))
  | first == '!'                          = rest
  | isAlphaNum first && T.all isGood name = f name
  where
    isGood c = isAlphaNum c || c == '-' || c `elem` cs
modifyFileWith _ _ name                   = name
