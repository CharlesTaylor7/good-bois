module DogCeo.Component.Breeds
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import DogCeo.Api.Breeds as BreedsApi
import DogCeo.Types (ApiResult(..), Breed, BreedGroup)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Output = Selected Breed

type Slot = forall query. H.Slot query Output Unit

type State =
  { breeds :: ApiResult (Array BreedGroup)
  }

data Action
  = FetchBreeds
  | Select Breed

component :: forall query input monad. MonadAff monad => H.Component query input Output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just FetchBreeds
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { breeds: Loading
  }

render :: forall monad. State -> H.ComponentHTML Action () monad
render state =
  HH.div
    []
    [ HH.h2 [ HP.class_ $ wrap "m-3 text-center text-2xl font-semibold" ] [ HH.text "Dog Breeds" ]
    , case state.breeds of
        Loading ->
          HH.text "Loading..."

        Success breeds ->
          HH.ul [ HP.class_ $ wrap "ml-7 flex flex-col flex-wrap items-start pl-4 h-[80vh] list-disc" ] $
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

handleAction :: forall slots monad. MonadAff monad => Action -> H.HalogenM State Action slots Output monad Unit
handleAction = case _ of
  FetchBreeds -> do
    breeds <- BreedsApi.fetch
    H.modify_ \state -> state
      { breeds = Success breeds }

  Select breed -> H.raise $ Selected breed
