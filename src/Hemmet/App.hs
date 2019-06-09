module Hemmet.App
    ( runCli
    , runTui
    ) where

import Control.Monad
import Data.Text (pack)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit
import System.IO

import Hemmet
import Hemmet.App.Cli
import Hemmet.App.TUI

runCli :: IO ()
runCli = do
    (Options input runner backend) <- execParser (cliWith optInput)
    let run' = run backend runner
    case input of
        StdIn -> T.getLine >>= run'
        Expression e -> run' $ pack e
        Examples ->
            forM_ (examples backend) $ \(desc, inp) -> do
                T.putStrLn $ "Example \"" <> desc <> "\""
                T.putStrLn "<<<"
                T.putStrLn inp
                T.putStrLn ">>>"
                run' inp
                T.putStrLn ""
                T.putStrLn ""
  where
    run backend runner input =
        case runHemmet backend runner input of
            Left err -> do
                T.putStr input -- echo an unchanged line
                hPutStr stderr $ errorBundlePretty err
                exitWith (ExitFailure 10)
            Right (Pure t) -> T.putStr t
            Right (Effect e) -> e >>= T.putStr

runTui :: IO ()
runTui = do
    (Options _ runner backend) <- execParser (cliWith $ pure ())
    tui (run backend runner)
  where
    run backend runner input =
        case runHemmet backend runner input of
            Left err -> pure . pack $ errorBundlePretty err
            Right (Pure t) -> pure t
            Right (Effect e) -> e
