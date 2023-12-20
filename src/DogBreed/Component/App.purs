module DogBreed.Component.App
  ( component
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Tuple.Nested ((/\))
import Deku.Common (Nut, pursx, text_, (!:=), (<#~>), (~~))
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState)
import Effect (Effect)
import Effect.Console as Console
import Record as Record
import Type.Proxy (Proxy(..))

data Page
  = BreedListPage
  | BreedDetailsPage String

type Breed =
  { name :: String
  , subBreeds :: Array String
  }

type ImageMap = Map String (Array String)

component :: Nut
component = Deku.do
  setPage /\ page <- useState BreedListPage
  -- TODO:
  -- Breed cache
  -- image cache

  let props = { setPage }
  page <#~> case _ of
    BreedListPage -> listView props
    BreedDetailsPage breed -> detailsView $ Record.insert (Proxy :: _ "breed") breed props

detailsView ::
  forall rest.
  { setPage :: Page -> Effect Unit
  , breed :: String
  | rest
  } ->
  Nut
detailsView props = Deku.do
  ( pursx ::
      _
        """
        <div class="flex flex-col items-center justify-center">
          <h2 class="text-xl font-semibold">Dog Breed Images</h2>
          <ul class="pl-4 list-disc">
          </ul>
        </div>
        """
  ) ~~ {}

defaultBreed :: Breed
defaultBreed = { name: "Hound", subBreeds: [ "Basset", "Daschan" ] }

listView ::
  forall rest.
  { setPage :: Page -> Effect Unit
  | rest
  } ->
  Nut
listView props = Deku.do
  ( pursx ::
      _
        """
        <div class="flex flex-col items-center justify-center">
          <h2 class="text-xl font-semibold">Dog Breeds</h2>
          <ul class="pl-4 list-disc">
              ~children~
          </ul>
        </div>
        """
  ) ~~
    { children: dogBreedComponent defaultBreed
    }

dogBreedComponent :: Breed -> Nut
dogBreedComponent breed =
  Deku.do
    ( pursx ::
        _
          """
            <li>
              <a 
                class="flex items-center p-2 cursor-pointer underline decoration-blue-400 text-sky-500"
                ~breedAttrs~
              >Hound</a>
              <ul class="pl-4 list-disc">
                ~subBreeds~
              </ul>
            </li>
        """
    )
    ~~
      { breedAttrs:
          D.OnClick !:=
            do

              Console.log $ "Hello " <> breed.name

      , subBreeds:
          breed.subBreeds # foldMap (\name -> (pursx :: _ "<li>~name~</li>") ~~ { name: text_ name })
      }
