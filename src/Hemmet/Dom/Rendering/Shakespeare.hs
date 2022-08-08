module Hemmet.Dom.Rendering.Shakespeare where

import Control.Monad
import Data.Foldable
import qualified Data.List as L
import qualified Data.Text as T

import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.Dom.Rendering.Common
import Hemmet.Dom.Tree

renderHamletM :: Renderer DomPayload
renderHamletM = run render
  where
    render (Node name (DomTag mbId classes childs)) = do
      let tagName = if name == "" then "div" else name
      pad
      out $ "<" <> tagName
      case mbId of
        Just x -> out $ "#" <> x
        _      -> pure ()
      for_ classes $ \cls ->
        out $ "." <> cls
      out ">"
      nl
      unless (L.null childs) $ do
        withOffset 2 $ traverse_ render childs
    render (Node _ (DomPlainText text)) = do
      pad
      out $ escaping text
      nl
    escaping text = suffix <> text <> postfix
      where
        suffix = if T.head text == ' ' then "\\" else ""
        postfix = if T.last text == ' ' then "#" else ""

renderCassiusM :: Renderer DomPayload
renderCassiusM = run (render . annotateLast . L.sort . allClasses)
  where
    render = traverse_ $ \(cls, isLast) -> do
      pad
      out $ "." <> cls
      nl
      unless isLast
        nl
