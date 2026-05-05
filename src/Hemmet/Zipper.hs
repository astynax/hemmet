{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Hemmet.Zipper where

import "mtl" Control.Monad.Reader

import Hemmet.Megaparsec

data StepParsing a = StepParsing
  { downP         :: SParser a ()
  , upP           :: SParser a ()
  , nextP         :: SParser a ()
  , leftBracketP  :: SParser a ()
  , rightBracketP :: SParser a ()
  , chunkP        :: SParser a a
  }

type SParser a = ParsecT ZippingError Text (Reader (StepParsing a))

parseWith
  :: Buildable a
     => StepParsing a -> Text -> Either (ParseErrorBundle Text ZippingError) [a]
parseWith sp t =
  let z = runParserT (stepsP (Zipper [] root []) <* eof) "" t `runReader` sp
  in reverse . children . top <$> z

stepsP :: forall a. Buildable a => Zipper a -> SParser a (Zipper a)
stepsP zz = block zz <|> steps zz
  where
    block, up, down, steps :: Zipper a -> SParser a (Zipper a)

    block z = do
      _ <- p leftBracketP
      bs <- reverse . children . top <$> stepsP (Zipper [] root [])
      let z' = zipBlock bs z
      _ <- p rightBracketP
      up z' <|> stepsP z' <|> pure z'

    steps z = do
      c <- p chunkP
      let z' = zipChunk c z
      (p nextP *> steps z') <|>
        up z' <|>
        down z' <|>
        block z' <|>
        pure z'

    up z = do
      ups <- some $ p upP
      z' <- go z ups
      stepsP z'
      where
        go :: Zipper a -> [b] -> SParser a (Zipper a)
        go x [] = pure x
        go x (_:ups) =
          case zipUp x of
            Left e  -> customFailure e
            Right y -> go y ups

    down z = do
      _ <- p downP
      let z' = zipDown z
      stepsP z'

    p :: (StepParsing a -> SParser a b) -> SParser a b
    p = join . lift . asks

data Zipper a = Zipper
  { stack    :: [(a, [a])]
  , current  :: a
  , children :: [a]
  }

class Buildable a where
  root :: a
  cons :: a -> [a] -> a

newtype ZippingError = ZippingError String deriving (Show, Eq, Ord)

instance ShowErrorComponent ZippingError where
  showErrorComponent (ZippingError e) = e

zipBlock :: [a] -> Zipper a -> Zipper a
zipBlock cs z = z { children = reverse cs <> children z }

zipUp :: Buildable a => Zipper a -> Either ZippingError (Zipper a)
zipUp (Zipper [] _ _) = Left $ ZippingError "Up over the top"
zipUp (Zipper ((c', ss') : ps) c ss) =
  Right $ Zipper ps c' (cons c (reverse ss) : ss')

zipChunk :: a -> Zipper a -> Zipper a
zipChunk c z = z { children = c : children z }

zipDown :: Zipper a -> Zipper a
zipDown (Zipper _ _ [])      = error "Impossible down-to-nothing"
zipDown (Zipper ps c (s:ss)) = Zipper ((c, ss) : ps) s []

top :: Buildable a => Zipper a -> Zipper a
top z = case zipUp z of
  Left _  -> z
  Right x -> top x
