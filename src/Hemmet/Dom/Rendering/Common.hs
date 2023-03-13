module Hemmet.Dom.Rendering.Common where

import Data.Foldable
import qualified Data.List as L
import Data.Text as T
import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.Dom.Tree

type NodeRenderer = Node DomPayload -> RendererM

quoted :: Text -> Text
quoted x = "\"" <> x <> "\""

listish :: [Text] -> Text
listish xs = "[" <> T.intercalate ", " xs <> "]"

run :: NodeRenderer -> Renderer DomPayload
run r = traverse_ r . _dpChilds

allClasses :: Node DomPayload -> [Text]
allClasses (Node _ (DomTag _ classes childs)) =
  L.nub $ classes <> L.concatMap allClasses childs
allClasses (Node _ (DomPlainText _)) = []

annotateLast :: [a] -> [(a, Bool)]
annotateLast xs = L.zip xs $ L.map (const False) (L.tail xs) <> [True]
