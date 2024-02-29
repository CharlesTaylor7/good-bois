module DogCeo.Component.Image
  ( component
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot id = forall query. H.Slot query Void id
type Input = { url :: String }

type State =
  { img_a :: String
  , img_b :: String
  , toggle :: Boolean
  }

data Action = Receive Input

component ::
  forall query output monad.
  H.Component query Input output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState { url } =
  { img_a: ""
  , img_b: url
  , toggle: false
  }

render ::
  forall monad slots.
  State ->
  H.ComponentHTML Action slots monad
render state =
  HH.div
    [ HP.class_ $ wrap "relative h-96 w-96" ]
    [ renderImage (if state.toggle then "opacity-100" else "opacity-0") state.img_a
    , renderImage (if state.toggle then "opacity-0" else "opacity-100") state.img_b
    ]

renderImage ::
  forall slots monad.
  String ->
  String ->
  H.ComponentHTML
    Action
    slots
    monad
renderImage className url =
  HH.img
    [ HP.src url
    , HP.class_ $ wrap $ Array.intercalate " "
        [ "absolute h-full w-full object-cover rounded transition-opacity"
        , className
        ]
    ]

handleAction ::
  forall output monad slots.
  Action ->
  H.HalogenM State Action slots output monad Unit
handleAction = case _ of
  Receive { url } ->
    H.modify_ $ \state ->
      if state.toggle then state { img_b = url, toggle = false }
      else state { img_a = url, toggle = true }
