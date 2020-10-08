module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit

import Hemmet
import Hemmet.Cli
import Hemmet.Tui

main :: IO ()
main = do
  Options output runner backend <- execParser $ cliWith optOutput
  tui (run backend runner) >>= \case
    Nothing     -> exitWith $ ExitFailure 1
    Just result -> case output of
      StdOut    -> TIO.putStr result
      File name -> TIO.writeFile name result
  where
    run backend runner input =
      case runHemmet backend runner input of
        Left err         -> pure . T.pack $ errorBundlePretty err
        Right (Pure t)   -> pure t
        Right (Effect e) -> e
