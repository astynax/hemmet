module Hemmet.Dom.Rendering.KotlinxHtml where

import Control.Monad
import Data.Foldable
import qualified Data.List as L

import Hemmet.Rendering
import Hemmet.Tree

import Hemmet.Dom.Rendering.Common
import Hemmet.Dom.Tree

renderKotlinxHtmlM :: Renderer DomPayload
renderKotlinxHtmlM = run render
  where
    render (Node name payload) = do
      let tagName = if name == "" then "div" else name
      pad
      out $ tagName <> " {"
      case payload of
        DomPayload Nothing [] Void       -> pure ()
        DomPayload mbId classes children -> do
          nl
          withOffset 4 $ do
            case mbId of
              Just x -> do
                pad
                out $"id = \"" <> x <> "\""
                nl
              Nothing -> pure ()
            unless (L.null classes) $ do
              pad
              out $ "classes = setOf("
                <> mconcat (L.intersperse ", " $ L.map quoted classes)
                <> ")"
              nl
            unless (L.null children) $ do
              traverse_ render children
          pad
      out "}"
      nl
