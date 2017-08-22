{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Data.Maybe
import Data.Monoid
import Data.Text (pack)
import Data.Text.IO as TIO (putStr)
import Options.Applicative
import System.Exit
import System.IO

import Hemmet

data Options where
    Options :: forall a. Backend a -> Runner a -> IO String -> Options

main :: IO ()
main = configure >>= run
  where
    configure = execParser cli
    run (Options backend runner input) = do
        line <- input
        case runHemmet backend runner (pack line) of
            Left err -> do
                Prelude.putStr line -- echo an unchanged line
                hPrint stderr err
                exitWith (ExitFailure 10)
            Right res ->
                case res of
                    Pure t -> TIO.putStr t
                    Effect e -> e

-- options
cli :: ParserInfo Options
cli =
    info
        (commands <**> helper)
        (progDesc "Expands the snippets" <>
         header "Hemmet, the snippet expander" <>
         fullDesc)

commands :: Parser Options
commands =
    subparser $
    mkCommand "bem" "Generates BEM markup" bem argBemRunner <>
    mkCommand "ftree" "Generates the file trees" fileTree argFileTreeRunner
  where
    mkCommand cmd desc backend argRunner =
        command cmd $
        info (Options backend <$> argRunner <*> optInput <**> helper) $
        progDesc desc

argBemRunner :: Parser BemRunner
argBemRunner = fromMaybe bemReactFlux <$> optional arg'
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

argFileTreeRunner :: Parser FileTreeRunner
argFileTreeRunner = fromMaybe treeLike <$> optional arg'
  where
    arg' = argument reader $ metavar "tree|bash"
    reader =
        eitherReader $ \raw ->
            case raw of
                "" -> Right treeLike
                "tree" -> Right treeLike
                "bash" -> Right bashScript
                _ -> Left $ "Unknown renderer: " ++ raw

optInput :: Parser (IO String)
optInput = maybe getLine pure <$> optional opt'
  where
    opt' =
        strOption
            (short 'e' <> long "expression" <> metavar "EXPRESSION" <>
             help "Expression (snippet) to expand")
