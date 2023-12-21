module DogCeo.Component.Breeds
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import DogCeo.Api.Breeds as BreedsApi
import DogCeo.Types (ApiResult(..), Breed)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Output = Clicked { breed :: String }

type Slot = forall query. H.Slot query Output Unit

type State =
  { breeds :: ApiResult (Array Breed)
  }

data Action
  = FetchBreeds
  | Click { breed :: String }

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
    [ HH.h2 [ HP.class_ $ wrap "text-xl font-semibold" ] [ HH.text "Dog Breeds" ]
    , case state.breeds of
        Loading ->
          HH.text "Loading..."

        Success breeds ->
          HH.ul [ HP.class_ $ wrap "pl-4 list-disc" ] $
            breeds <#> \breed ->
              HH.li []
                [ HH.a
                    [ HP.class_ $ wrap "flex cursor-pointer underline decoration-blue-400 text-sky-500"
                    , HE.onClick \_ -> Click { breed: breed.name }
                    ]
                    [ HH.text breed.name ]
                , HH.ul [ HP.class_ $ wrap "pl-4 list-disc" ] $
                    breed.subBreeds <#> \name ->
                      HH.li [] [ HH.text name ]
                ]
    ]

{-

          <h2 class="text-xl font-semibold">Dog Breeds</h2>
          <ul class="pl-4 list-disc">
              ~children~
          </ul>
 
<li>
              <a 
                class="flex items-center p-2 cursor-pointer underline decoration-blue-400 text-sky-500"
                ~onClick~
              >
                ~name~
             

   -}

type DogBreedResponse =
  { message :: FO.Object (Array String)
  }

handleAction :: forall slots monad. MonadAff monad => Action -> H.HalogenM State Action slots Output monad Unit
handleAction = case _ of
  FetchBreeds -> do
    breeds <- BreedsApi.fetch
    H.modify_ \state -> state
      { breeds = breeds }

  Click args -> H.raise $ Clicked args
