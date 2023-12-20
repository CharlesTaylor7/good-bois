module Utils where

import Prelude

import Debug (class DebugWarning, spy)
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event as Event

-- | copied from FRP.Event v2.4.0
bindToEffect :: forall a b. Event a -> (a -> Effect b) -> Event b
bindToEffect e f =
  Event.makeEvent \k -> do
    u <- Event.subscribe e (f >=> k)
    pure u

monitor :: forall a. DebugWarning => String -> Event a -> Event a
monitor tag event = event `bindToEffect` \a -> pure $ spy tag a
