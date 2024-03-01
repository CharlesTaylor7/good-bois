--| Renders a particular image in the image viewer grid.
--| Responsible for cross fading with the previous image or loading state.
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
  { img_a :: Image
  , img_b :: Image
  , active :: Option
  }

data Action
  = Receive Input
  | Load Option

type Image = { loading :: Boolean, src :: String }
data Option = A | B

derive instance Eq Option

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
  { img_a: { loading: true, src: url }
  , img_b: { loading: false, src: "" }
  , active: A
  }

render ::
  forall monad slots.
  State ->
  H.ComponentHTML Action slots monad
render state =
  HH.div
    [ HP.class_ $ wrap "relative h-96 w-96" ]
    $
      join
        [ renderImage { option: A, state }
        , renderImage { option: B, state }
        ]

renderImage ::
  forall slots monad.
  { option :: Option, state :: State } ->
  Array
    ( H.ComponentHTML
        Action
        slots
        monad
    )
renderImage { option, state } =
  [ HH.img
      [ HP.src img.src
      , HE.onLoad \_ -> Load option
      , HP.class_ $ wrap $ Array.intercalate " "
          [ "absolute h-full w-full object-cover rounded transition-opacity"
          , if option == state.active && not img.loading then "opacity-100" else "opacity-0"
          ]
      ]
  , HH.img
      [ HP.src "/good-bois/static/loading.gif"
      , HP.class_ $ wrap $ Array.intercalate " "
          [ "absolute h-full w-full object-cover rounded transition-opacity"
          , if option == state.active && img.loading then "opacity-100" else "opacity-0"
          ]
      ]
  ]
  where
  img = case option of
    A -> state.img_a
    B -> state.img_b

{-
renderImage ::
  forall slots monad.
  { option :: Option, state :: State } ->
  H.ComponentHTML
    Action
    slots
    monad
renderImage { option, state } =
  HH.img
    [ HP.src img.src
    , HP.class_ $ wrap $ Array.intercalate " "
        [ "absolute h-full w-full object-cover rounded transition-opacity"
        , className
        ]
    ]
  where
  className = if state.active == option then "opacity-100" else "opacity-0"
  -}

handleAction ::
  forall output monad slots.
  Action ->
  H.HalogenM State Action slots output monad Unit
handleAction = case _ of
  Load option ->
    H.modify_ \state ->
      case option of
        A -> state { img_a { loading = false } }
        B -> state { img_b { loading = false } }

  Receive { url } ->
    H.modify_ \state ->
      case state.active of
        A ->
          state
            { img_b = { loading: true, src: url }
            , active = B
            }
        B ->
          state
            { img_a = { loading: true, src: url }
            , active = A
            }
