module Hemmet.App
  ( runCli
  , runTui
  ) where

import Prelude hiding (putStrLn, putStr, getLine, writeFile)
import Control.Monad
import Data.Text (pack)
import Data.Text.IO (putStrLn, putStr, getLine, writeFile)
import Options.Applicative
import System.Exit
import System.IO (hPutStr, stderr)

import Hemmet
import Hemmet.App.Cli
import Hemmet.App.TUI

runCli :: IO ()
runCli = do
  (Options input runner backend) <- execParser (cliWith optInput)
  let run' = run backend runner
  case input of
    StdIn        -> getLine >>= run'
    Expression e -> run' $ pack e
    Examples     ->
      forM_ (examples backend) $ \(desc, inp) -> do
        putStrLn $ "Example \"" <> desc <> "\""
        putStrLn "<<<"
        putStrLn inp
        putStrLn ">>>"
        run' inp
        putStrLn ""
        putStrLn ""
  where
    run backend runner input =
      case runHemmet backend runner input of
        Left err         -> do
          putStr input -- echo an unchanged line
          hPutStr stderr $ errorBundlePretty err
          exitWith $ ExitFailure 10
        Right (Pure t)   -> putStr t
        Right (Effect e) -> e >>= putStr

runTui :: IO ()
runTui = do
  (Options output runner backend) <- execParser (cliWith optOutput)
  tui (run backend runner) >>= \case
    Nothing     -> exitWith $ ExitFailure 1
    Just result -> case output of
      StdOut    -> putStr result
      File name -> writeFile name result
      Cmd _     -> putStrLn "TODO"
  where
    run backend runner input =
      case runHemmet backend runner input of
        Left err         -> pure . pack $ errorBundlePretty err
        Right (Pure t)   -> pure t
        Right (Effect e) -> e
