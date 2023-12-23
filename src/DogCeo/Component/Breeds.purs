module DogCeo.Component.Breeds
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import DogCeo.Routes (Route(..))
import DogCeo.Types (ApiResult(..), Breed, BreedGroup)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.Class as HR

data Output = Void

type Slot = forall query. H.Slot query Output Unit

type Input = State

type State =
  { breeds :: ApiResult (Array BreedGroup)
  }

data Action
  = Select Breed
  | Receive Input

component ::
  forall query monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  H.Component query Input Output monad
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
initialState input = input

render :: forall monad. State -> H.ComponentHTML Action () monad
render state =
  HH.div
    []
    [ HH.h2 [ HP.class_ $ wrap "m-3 text-center text-2xl font-semibold" ] [ HH.text "Dog Breeds" ]
    , case state.breeds of
        Loading ->
          HH.div
            [ HP.class_ $ wrap "flex justify-center"
            ]
            [ HH.img
                [ HP.src "./static/loading.gif"
                , HP.alt "Loading"
                ]
            ]

        Success breeds ->
          HH.ul [ HP.class_ $ wrap "ml-7 sm:flex flex-col flex-wrap items-start pl-4 h-[80vh] list-disc" ] $
            breeds <#> \breed ->
              HH.li [ HP.class_ $ wrap "" ]
                [ HH.a
                    [ anchorStyle
                    , HE.onClick \_ -> Select { name: breed.name, subBreed: Nothing }
                    ]
                    [ HH.text breed.name ]
                , HH.ul [ HP.class_ $ wrap "pl-4 list-circle" ] $
                    breed.subBreeds <#> \name ->
                      HH.li []
                        [ HH.a
                            [ anchorStyle
                            , HE.onClick \_ -> Select { name: breed.name, subBreed: Just name }
                            ]
                            [ HH.text name ]
                        ]
                ]
    ]

  where
  anchorStyle = HP.class_ $ wrap "cursor-pointer underline decoration-blue-400 text-sky-500"

handleAction ::
  forall slots monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  Action ->
  H.HalogenM State Action slots Output monad Unit
handleAction = case _ of
  Receive input -> H.put input

  Select breed -> do
    HR.navigate $ ImagesRoute { breed, page: 1 }
