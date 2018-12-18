module Main where

import Prelude

import App as App
import Control.Coroutine as CR
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody

  -- loop on Query with initializer
  runUI App.component unit body

  -- -- forever querying
  -- app <- runUI App.component unit body
  -- forever do
  --   delay (Milliseconds 30.0)
  --   app.query $ H.action App.Increment

  -- -- infinite coroutine
  -- app <- runUI App.component unit body
  -- CR.runProcess (producer CR.$$ consumer app)
  -- where
  -- producer = CR.producer $ Left unit <$ delay (Milliseconds 30.0)
  -- consumer app = CR.consumer $ const $ Nothing <$ app.query (H.action App.Increment)
