{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Text (pack)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Exit
import System.IO

import Hemmet hiding (Parser)

data Input
    = Expression String
    | Examples
    | StdIn

data Options where
    Options :: forall a. Input -> Runner a -> Backend a -> Options

main :: IO ()
main = do
    (Options input runner backend) <- execParser cli
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
                hPutStr stderr $ parseErrorPretty' input err
                exitWith (ExitFailure 10)
            Right res ->
                case res of
                    Pure t -> T.putStr t
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
    cmd "bem" (bem <&> argBemRunner) "Generates BEM markup" <>
    cmd "ftree" (fileTree <&> argFileTreeRunner) "Generates the file trees"

argBemRunner :: Parser (BemBackend -> Options)
argBemRunner = subparser $ flux <> html <> css
  where
    flux =
        cmd "react-flux" (bemReactFlux <&> optInput) "Generates react-flux code"
    html = cmd "html" (bemHtml <&> optInput) "Generates HTML"
    css = cmd "css" (bemCss <&> optInput) "Generates CSS"

argFileTreeRunner :: Parser (FileTreeBackend -> Options)
argFileTreeRunner = subparser $ tree <> bash
  where
    tree = cmd "tree" (treeLike <&> optInput) "Generates GNU Tree-like output"
    bash = cmd "bash" (bashScript <&> optInput) "Generates Bash-script"

optInput :: Parser (Runner a -> Backend a -> Options)
optInput =
    Options . fromMaybe StdIn <$> (optional example <|> optional expression)
  where
    expression =
        Expression <$>
        strOption
            (short 'e' <> long "expression" <> metavar "EXPRESSION" <>
             help "Expression (snippet) to expand")
    example = flag' Examples (long "examples" <> help "Show some examples")

cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd c p desc = command c $ info (p <**> helper) $ progDesc desc

(<&>) :: Functor f => a -> f (a -> b) -> f b
x <&> f = ($ x) <$> f
