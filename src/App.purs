module App where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (liftAff, modify_)
import Halogen as H
import Halogen.HTML as HH

----------------------------------------------------------------

type State = Int

data Query a
  = Increment a
  | Loop a

type Slot = (child :: ChildSlot Int)

_child = SProxy :: SProxy "child"

----------------------------------------------------------------
component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.component
    { initialState: const 0
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Loop -- loop on Query
    -- , initializer: Nothing
    , finalizer: Nothing
    }
  where
  render s =
    HH.div_
      [ HH.slot _child (s `mod` 3) child s absurd ]

  eval :: Query ~> H.HalogenM State Query Slot Void Aff
  eval = case _ of
    Increment next -> do
      modify_ (_ + 1)
      pure next

    -- simple recursion
    Loop next -> do
      liftAff $ delay (Milliseconds 30.0)
      eval <<< Loop <=< eval <<< Increment $ next

    -- -- MonadRec
    -- Loop next -> forever do
    --   liftAff $ delay (Milliseconds 30.0)
    --   eval $ H.action Increment

----------------------------------------------------------------

type CQuery = Const Void

type ChildSlot = H.Slot CQuery Void

----------------------------------------------------------------

child :: H.Component HH.HTML CQuery State Void Aff
child =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
  render s =
    HH.p_
      [ HH.text $ show s ]

  eval :: CQuery ~> H.HalogenM State CQuery () Void Aff
  eval = case _ of Const void -> absurd void
