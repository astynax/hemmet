module Hemmet.Dom
  ( DomBackend
  , DomRunner
  , dom
  , domCss
  , domElm
  , domHtml
  , domLucid
  , domHamlet
  , domCassius
  , domKotlinxHtml
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

domLucid :: DomRunner
domLucid = PureRunner renderLucidM

domHamlet :: DomRunner
domHamlet = PureRunner renderHamletM

domCassius :: DomRunner
domCassius = PureRunner renderCassiusM

domKotlinxHtml :: DomRunner
domKotlinxHtml = PureRunner renderKotlinxHtmlM

domExamples :: [(Text, Text)]
domExamples =
  [ ( "Simple", "html>head+body>h1.red+p#article" )
  , ( "Typical"
    , "#root>.nav>(ul.menu>li.item+li.item)+#content+.footer>img.logo"
    )
  ]
