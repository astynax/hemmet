module Hemmet.Dom.Rendering
  ( renderHtmlM
  , renderLucidM
  , renderCssM
  , renderElmM
  , module KotlinxHtml
  , module Shakespeare
  ) where

import Control.Monad
import Data.Foldable
import qualified Data.List as L
import Data.Text as T

import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.Dom.Rendering.Common
import Hemmet.Dom.Rendering.KotlinxHtml as KotlinxHtml
import Hemmet.Dom.Rendering.Shakespeare as Shakespeare
import Hemmet.Dom.Tree

renderHtmlM :: Renderer DomPayload
renderHtmlM = run renderHtmlM'

renderHtmlM' :: NodeRenderer
renderHtmlM' (Node name (DomPayload mbId classes childs)) = do
  let tagName = if name == "" then "div" else name
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

renderCssM :: Renderer DomPayload
renderCssM = run renderCssM'

renderCssM' :: NodeRenderer
renderCssM' = render . annotateLast . L.sort . allClasses
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

renderElmM :: Renderer DomPayload
renderElmM = run $ renderElmM' pad

renderElmM' :: RendererM -> NodeRenderer
renderElmM' fstPad (Node name (DomPayload mbId classes childs)) = do
  let tagName = if name == "" then "div" else name
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
    tagId = ["id " <> quoted x | Just x <- [mbId]]
    tagClasses = L.map (("class " <>) . quoted) classes
    tagAttrs = case tagId <> tagClasses of
      [] -> "[]"
      as -> "[ " <> T.intercalate ", " as <> " ]"

renderLucidM :: Renderer DomPayload
renderLucidM = run renderLucidM'

renderLucidM' :: NodeRenderer
renderLucidM' (Node name (DomPayload mbId classes childs)) = do
  let tagName = if name == "" then "div_" else name <> "_"
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
    attrs = L.concat
      [["id_ " <> quoted i] | Just i <- [mbId]] <> (
        case L.nub classes of
          []  -> []
          [x] -> ["class_ " <> quoted x]
          xs  -> ["classes_ " <> listish (L.map quoted xs)]
        )
