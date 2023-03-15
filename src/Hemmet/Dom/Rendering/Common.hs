module Hemmet.Dom.Rendering.Common where

import Data.Foldable
import qualified Data.List as L
import Data.Text as T
import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.Dom.Tree as Tree

type NodeRenderer = Node DomPayload -> RendererM

quoted :: Text -> Text
quoted x = "\"" <> x <> "\""

listish :: [Text] -> Text
listish xs = "[" <> T.intercalate ", " xs <> "]"

run :: NodeRenderer -> Renderer DomPayload
run r = traverse_ r . _dpChildren

allClasses :: Node DomPayload -> [Text]
allClasses (Node _ (DomPayload _ classes children)) =
  L.nub $ classes <> L.concatMap allClasses children

annotateLast :: [a] -> [(a, Bool)]
annotateLast [] = error "Impossible"
annotateLast xs@(_:ts) =
  L.zip xs $ L.map (const False) ts <> [True]
