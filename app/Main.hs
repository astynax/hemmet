{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Text (pack)
import Data.Text.IO as TIO (putStr)
import Options.Applicative
import System.Exit
import System.IO

import Hemmet

data Options = Options
    { runner :: BemRunner
    , input :: IO String
    }

main :: IO ()
main = configure >>= run
  where
    configure = execParser cli
    run opts = do
        line <- input opts
        case runHemmet bem (runner opts) (pack line) of
            Left err -> do
                Prelude.putStr line -- echo an unchanged line
                hPutStrLn stderr $ show err
                exitWith (ExitFailure 10)
            Right res ->
                case res of
                    Pure t -> TIO.putStr t
                    Effect e -> e

-- options
cli :: ParserInfo Options
cli =
    info
        (options <**> helper)
        (progDesc "Expands the template string" <>
         header "Hemmet, the snippet expander" <>
         fullDesc)

options :: Parser Options
options = Options <$> argRunner <*> optInput

argRunner :: Parser BemRunner
argRunner = fromMaybe bemReactFlux <$> optional arg'
  where
    arg' = argument reader $ metavar "html|css|react-flux"
    reader =
        eitherReader $ \raw ->
            case raw of
                "" -> Right bemReactFlux
                "react-flux" -> Right bemReactFlux
                "html" -> Right bemHtml
                "css" -> Right bemCss
                _ -> Left $ "Unknown renderer: " ++ raw

optInput :: Parser (IO String)
optInput = maybe getLine pure <$> optional opt'
  where
    opt' =
        strOption
            (short 'e' <> long "expression" <> metavar "EXPRESSION" <>
             help "Expression (snippet) to expand")
