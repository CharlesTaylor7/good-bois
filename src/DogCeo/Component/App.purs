module DogCeo.Component.App
  ( component
  ) where

import Prelude

import Data.Newtype (wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { counter :: Int }

data Action = Increment

component :: forall query input output monad. H.Component query input output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = { counter: 0 }

render :: forall monad. State -> H.ComponentHTML Action () monad
render state =
  let
    label = show state.counter
  in
    HH.div
      [ HP.class_ $ wrap "mt-3 flex flex-col justify-center items-center" ]

      [ HH.button
          [ HP.title label
          , HE.onClick \_ -> Increment
          , HP.class_ $ wrap "border rounded-lg py-1 px-3 bg-sky-300"
          ]
          [ HH.text label ]
      ]

handleAction :: forall output monad. Action -> H.HalogenM State Action () output monad Unit
handleAction = case _ of
  Increment ->
    H.modify_ \state -> state { counter = state.counter + 1 }
