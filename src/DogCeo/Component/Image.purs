module DogCeo.Component.Image
  ( component
  , Input
  , InputRow
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record

type Input = Record InputRow
type InputRow =
  ( current :: String
  , next :: Maybe String
  )

type Slot = forall query. H.Slot query Output Int

type State =
  { imageNotFound :: Boolean
  | InputRow
  }

data Output = Void

data Action
  = Receive Input
  | ImageNotFound

component :: forall query monad. H.Component query Input Output monad
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
initialState =
  Record.merge
    { imageNotFound: false
    }

render :: forall monad. State -> H.ComponentHTML Action () monad
render { current, next, imageNotFound } =
  HH.span
    [ HP.class_ $ wrap "inline-block" ]
    [ HH.img
        [ HP.src current
        , HE.onError \_ -> ImageNotFound
        , HP.class_ $ wrap $ Array.intercalate " "
            [ "object-cover h-96 rounded"
            , if imageNotFound then "hidden" else ""
            ]
        ]
    , HH.link
        [ HP.rel "prefetch"
        , HP.href $ next # fromMaybe ""
        ]
    ]

handleAction :: forall slots monad. Action -> H.HalogenM State Action slots Output monad Unit
handleAction =
  case _ of
    Receive input ->
      H.modify_ $ Record.merge input

    ImageNotFound ->
      H.modify_ $ \state -> state
        { imageNotFound = true }
