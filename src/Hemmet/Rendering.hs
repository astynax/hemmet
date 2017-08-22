module Hemmet.Rendering
    ( Renderer
    , RendererM
    , runRenderM
    , pad
    , nl
    , out
    , paddedWith
    , withOffset
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Text as T hiding (null)

import Hemmet.Tree

type RendererM = ReaderT Text (Writer Text) ()

type Renderer a = Tree a -> RendererM

runRenderM :: RendererM -> Text -> Text
runRenderM renderer = execWriter . runReaderT renderer

pad :: RendererM
pad = ask >>= lift . tell

nl :: RendererM
nl = lift $ tell "\n"

out :: Text -> RendererM
out = lift . tell

paddedWith :: Text -> RendererM -> RendererM
paddedWith p = withReaderT (<> p)

withOffset :: Int -> RendererM -> RendererM
withOffset = paddedWith . (`T.replicate` " ")
