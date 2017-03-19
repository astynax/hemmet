{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hemmet.Rendering
    ( Renderer
    , RendererM
    , runRenderM
    , pad
    , nl
    , out
    , withOffset
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Text as T hiding (null)

import Hemmet.Tree

type RendererM = ReaderT Int (Writer Text) ()

type Renderer a = Tree a -> RendererM

runRenderM :: RendererM -> Int -> Text
runRenderM renderer offset = execWriter $ runReaderT renderer offset

pad :: RendererM
pad = ask >>= \n -> lift (tell $ T.replicate n " ")

nl :: RendererM
nl = lift $ tell "\n"

out :: Text -> RendererM
out = lift . tell

withOffset :: Int -> RendererM -> RendererM
withOffset n = withReaderT (+ n)
