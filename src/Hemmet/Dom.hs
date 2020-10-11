module Hemmet.Dom
  ( DomBackend
  , DomRunner
  , dom
  , domCss
  , domElm
  , domHtml
  ) where

import Data.Text as T

import Hemmet.Backend
import Hemmet.Runner

import Hemmet.Dom.Rendering
import Hemmet.Dom.Template
import Hemmet.Dom.Tree

type DomBackend = Backend DomPayload

type DomRunner = Runner DomPayload

dom :: DomBackend
dom =
  Backend
    { getTransformation = (id,)
    , parser = template
    , examples = domExamples
    }

domCss :: DomRunner
domCss = PureRunner renderCssM

domElm :: DomRunner
domElm = PureRunner renderElmM

domHtml :: DomRunner
domHtml = PureRunner renderHtmlM

domExamples :: [(Text, Text)]
domExamples =
  [ ( "Simple", "html>head+body>h1.red+p#article" )
  , ( "Typical"
    , "#root>.nav>(ul.menu>li.item+li.item)+#content+.footer>img.logo"
    )
  ]
