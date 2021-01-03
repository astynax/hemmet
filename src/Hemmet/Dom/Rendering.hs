module Hemmet.Dom.Rendering
  ( renderHtmlM
  , renderLucidM
  , renderCssM
  , renderElmM
  ) where

import Control.Monad
import Data.Foldable
import Data.List as L
import Data.Text as T

import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.Dom.Tree

type NodeRenderer = Node DomPayload -> RendererM

renderHtmlM :: Renderer DomPayload
renderHtmlM = run renderHtmlM'

renderHtmlM' :: NodeRenderer
renderHtmlM' (Node name (DomPayload mbId classes childs)) = do
  pad
  out $ "<" <> tagName
  traverse_ out
    [" id=" <> quoted x | Just x <- [mbId]]
  unless (L.null classes) $
    out $ " class=" <> quoted (T.unwords classes)
  out ">"
  unless (L.null childs) $ do
    nl
    withOffset 2 $ traverse_ renderHtmlM' childs
    pad
  out ("</" <> tagName <> ">")
  nl
  where
    tagName =
      case name of
        "" -> "div"
        _  -> name

renderCssM :: Renderer DomPayload
renderCssM = run renderCssM'

renderCssM' :: NodeRenderer
renderCssM' = render . annotate . sort . collect
  where
    render = traverse_ $ \(cls, isLast) -> do
      pad
      out ("." <> cls <> " {")
      nl
      pad
      out "}"
      nl
      unless isLast
        nl
    annotate [] = []
    annotate xs@(_:rest) = L.zip xs $ L.map (const False) rest <> [True]
    collect (Node _ (DomPayload _ classes childs)) =
      L.nub $ classes <> L.concatMap collect childs

renderElmM :: Renderer DomPayload
renderElmM = run $ renderElmM' pad

renderElmM' :: RendererM -> NodeRenderer
renderElmM' fstPad (Node name (DomPayload mbId classes childs)) = do
  fstPad >> out (tagName <> " " <> tagAttrs)
  case childs of
    []     -> do
      out " []"
      nl
    (c:cs) -> do
      nl
      withOffset 4 $ do
        renderElmM' (pad >> out "[ ") c
        traverse_ (renderElmM' $ pad >> out ", ") cs
        pad
        out "]"
        nl
  where
    tagName =
      case name of
        "" -> "div"
        _  -> name
    tagId = ["id " <> quoted x | Just x <- [mbId]]
    tagClasses = L.map (("class " <>) . quoted) classes
    tagAttrs = case tagId <> tagClasses of
      [] -> "[]"
      as -> "[ " <> T.intercalate ", " as <> " ]"

renderLucidM :: Renderer DomPayload
renderLucidM = run renderLucidM'

renderLucidM' :: NodeRenderer
renderLucidM' (Node name (DomPayload mbId classes childs)) = do
  pad
  out tagName
  unless (L.null attrs) $
    out $ " " <> listish attrs
  unless (L.null childs) $ do
    out " $ do"
    nl
    withOffset 2 $ traverse_ renderLucidM' childs
  when (L.null attrs && L.null childs) $ do
    out " []"
  when (L.null childs) nl
  where
    tagName =
      case name of
        "" -> "div_"
        _  -> name <> "_"
    attrs = L.concat
      [["id_ " <> quoted i] | Just i <- [mbId]] <> (
        case L.nub classes of
          []  -> []
          [x] -> ["class_ " <> quoted x]
          xs  -> ["classes_ " <> listish (L.map quoted xs)]
        )

quoted :: Text -> Text
quoted x = "\"" <> x <> "\""

listish :: [Text] -> Text
listish xs = "[" <> T.intercalate ", " xs <> "]"

run :: NodeRenderer -> Renderer DomPayload
run r = traverse_ r . _dpChilds
