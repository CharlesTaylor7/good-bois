module Deku.Extra
  ( Pusher
  , module FRP.Event
  , module Web.UIEvent.KeyboardEvent
  ) where

import Prelude

import Effect (Effect)
import FRP.Event (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

type Pusher a = a -> Effect Unit
