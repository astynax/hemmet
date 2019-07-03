module Golden
  ( makeGoldenTests
  ) where

import Control.Monad
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL hiding (ByteString)
import Data.Text as T
import Data.Text.Encoding
import System.FilePath
import System.FilePath.Glob
import Test.Tasty
import Test.Tasty.Golden

import Hemmet
import Hemmet.BEM.Tree
import Hemmet.FileTree.Tree

type GoldenSuite a = (Backend a, [(Runner a, String)])

makeGoldenTests :: FilePath -> IO [TestTree]
makeGoldenTests root =
  sequence
    [ testDirWith (root </> "bem") goldenBem
    , testDirWith (root </> "ftree") goldenFileTree
    ]

goldenBem :: GoldenSuite BemPayload
goldenBem =
  ( bem
  , [ (bemHtml, ".html")
    , (bemCss, ".css")
    , (bemReactFlux, ".react-flux")
    ]
  )

goldenFileTree :: GoldenSuite FileTreePayload
goldenFileTree =
  ( fileTree
  , [ (treeLike, ".tree")
    , (bashScript, ".bash")
    ]
  )

testDirWith :: FilePath -> GoldenSuite a -> IO TestTree
testDirWith dir (backend, renderers) = do
  inputFiles <- globDir1 (compile "*.hemmet") dir
  fmap (testGroup dir) . forM inputFiles $ \path -> do
    input <- BS.readFile path
    let
      baseName                 = takeBaseName path
      check (renderer, suffix) =
        let goldenFile = replaceExtension path $ suffix <> ".golden"
        in goldenVsStringDiff
          (baseName <> suffix)
          (\gold new -> ["diff", "-u", gold, new])
          goldenFile
          (return . BSL.fromStrict . run backend renderer $ input)
    return . testGroup baseName $ Prelude.map check renderers

run :: Backend a -> Runner a -> ByteString -> ByteString
run backend runner = encodeUtf8 . run' . Prelude.head . T.lines . decodeUtf8
  where
    run' = either (T.pack . show) fromResult . runHemmet backend runner
    fromResult (Pure t) = t
    fromResult _ = error "Unexpected effectful result!"
