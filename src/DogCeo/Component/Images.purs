module DogCeo.Component.Images
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import DogCeo.Types (ApiResult(..), Breed)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record

type Input =
  { breed :: Breed
  , images :: ApiResult (Array String)
  }

data Output = ToListView

type Slot = forall query. H.Slot query Output Unit

type State =
  { breed :: Breed
  , images :: ApiResult (Array String)
  }

data Action = Breadcrumb

component :: forall query monad. MonadAff monad => H.Component query Input Output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

initialState :: Input -> State
initialState input = Record.merge input {}

render :: forall monad. State -> H.ComponentHTML Action () monad
render _state =
  HH.div
    []
    [ HH.h2 [ HP.class_ $ wrap "text-xl font-semibold" ] [ HH.text "Dog Images" ]

    , HH.a
        [ HP.class_ $ wrap "flex items-center p-2 cursor-pointer underline decoration-blue-400 text-sky-500"
        , HE.onClick \_ -> Breadcrumb
        ]
        [ HH.text "Back to Breeds"
        ]
    ]

handleAction :: forall slots monad. MonadAff monad => Action -> H.HalogenM State Action slots Output monad Unit
handleAction =
  case _ of
    Breadcrumb -> H.raise ToListView

