module App where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (defaultEval, liftAff, mkEval, modify_)
import Halogen as H
import Halogen.HTML as HH

----------------------------------------------------------------

type State = Int

type Query = Const Void

data Action
  = Increment
  | Loop

type Slot = (child :: ChildSlot Int)

_child = SProxy :: SProxy "child"

----------------------------------------------------------------

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.mkComponent
    { initialState: const 0
    , render
    , eval: mkEval defaultEval
        { handleAction = action
        , initialize = Just Loop -- loop on Query
        }
    }
  where
  render s =
    HH.div_
      [ HH.slot _child (s `mod` 3) child s absurd ]
      -- [ HH.p_ [ HH.text $ show s ] ] -- without child component is ok

  action :: Action -> H.HalogenM State Action Slot Void Aff Unit
  action = case _ of
    Increment -> do
      modify_ (_ + 1)

    Loop -> forever do
      liftAff $ delay (Milliseconds 30.0)
      action Increment

----------------------------------------------------------------

type CQuery = Const Void

type ChildSlot = H.Slot CQuery Void

----------------------------------------------------------------

child :: H.Component HH.HTML CQuery State Void Aff
child =
  H.mkComponent
    { initialState: identity
    , render
    , eval: mkEval defaultEval
    }
  where
  render s =
    HH.p_
      [ HH.text $ show s ]
