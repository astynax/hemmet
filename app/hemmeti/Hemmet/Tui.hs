module Hemmet.Tui
  ( tui
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Control.Monad.Trans
import Data.Text
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Lens.Micro

data Focus
  = Input
  | Output
  deriving (Eq, Ord, Show)

data State = State
  { sEditor :: Editor Text Focus
  , sOutput :: Text
  , sFocus  :: Focus
  , sDone   :: Bool
  }

editorL :: Lens' State (Editor Text Focus)
editorL = lens sEditor (\s e -> s{sEditor = e})

type Evaluator = Text -> IO Text

tui :: Evaluator -> IO (Maybe Text)
tui run = do
  s <- defaultMain app State
    { sEditor = editorText Input (Just 1) ""
    , sOutput = ""
    , sFocus  = Input
    , sDone   = False
    }
  pure $ if sDone s then Just (sOutput s) else Nothing
  where
    app :: App State Event Focus
    app = App
      { appDraw         = draw
      , appChooseCursor = chooseCursor
      , appHandleEvent  = handle run
      , appStartEvent   = pure ()
      , appAttrMap      = const . setBorderColor blue $ attrMap defAttr []
      }

draw :: State -> [Widget Focus]
draw State {..} = [input <=> output]
  where
    decorate f
      | sFocus == f = updateAttrMap $ setBorderColor yellow
      | otherwise   = id
    input = decorate Input $ border $ vLimit 1
      $ renderEditor re (sFocus == Input) sEditor
    output = decorate Output $ border $ viewport Output Both $ txt sOutput
    re (t:_) = txt t
    re []    = txt ""

handle
  :: Evaluator
  -> BrickEvent Focus Event
  -> EventM Focus State ()
handle eval be@(VtyEvent e) = do
  s@State {sFocus} <- get
  case e of
    (EvKey KEnter [])           -> put s{sDone = True} >> halt
    (EvKey KEsc [])             -> halt
    (EvKey (KChar 'c') [MCtrl]) -> halt
    (EvKey (KChar 'g') [MCtrl]) -> halt
    (EvKey (KChar '\t') [])     ->
      put s
        { sFocus =
             case sFocus of
               Input  -> Output
               Output -> Input
        }
    _ ->
      case sFocus of
        Input -> do
          zoom editorL $ handleEditorEvent be
          ed' <- gets sEditor
          out' <- liftIO . eval . mconcat . getEditContents $ ed'
          put s {sEditor = ed', sOutput = out'}
        Output -> do
          case e of
            (EvKey KUp [])    -> vScrollBy vps (-1)
            (EvKey KDown [])  -> vScrollBy vps 1
            (EvKey KLeft [])  -> hScrollBy vps (-1)
            (EvKey KRight []) -> hScrollBy vps 1
            _ -> pure ()
          put s
  where
    vps = viewportScroll Output
handle _ _ = pure ()

setBorderColor :: Color -> AttrMap -> AttrMap
setBorderColor c = applyAttrMappings [(borderAttr, defAttr `withForeColor` c)]

chooseCursor :: State -> [CursorLocation Focus] -> Maybe (CursorLocation Focus)
chooseCursor (sFocus -> Input) ps =
  case [p | p <- ps, cursorLocationName p == Just Input] of
    (x:_) -> Just x
    _     -> Nothing
chooseCursor _ _ = Nothing
