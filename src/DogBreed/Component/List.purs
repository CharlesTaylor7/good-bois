module DogBreed.Component.List where

{-
import Prelude

import Data.Foldable (foldMap)
import Deku.Common (Nut, pursx, text_, useState, (!:=), (<#~>), (~~))
import Deku.DOM as D
import Deku.Do as Deku
import DogBreed.Types (ApiResult(..), Breed, Page(..))
import Effect (Effect)
import Effect.Console as Console
import FRP.Event (Event)

defaultBreed :: Breed
defaultBreed = { name: "Hound", subBreeds: [ "Basset", "Daschan" ] }

component ::
  forall rest.
  { breeds :: Event (ApiResult (Array Breed))
  , setPage :: Page -> Effect Unit
  | rest
  } ->
  Nut
component props = Deku.do
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
    { children:
        props.breeds <#~>
          case _ of
            Loading -> text_ "Loading..."
            Success breeds ->
              breeds # foldMap \breed -> dogBreedComponent { breed, setPage: props.setPage }
    }

dogBreedComponent ::
  forall rest.
  { breed :: Breed
  , setPage :: Page -> Effect Unit
  | rest
  } ->
  Nut
dogBreedComponent props =
  Deku.do
    ( pursx ::
        _
          """
            <li>
              <a 
                class="flex items-center p-2 cursor-pointer underline decoration-blue-400 text-sky-500"
                ~onClick~
              >
                ~name~
              </a>
              <ul class="pl-4 list-disc">
                ~subBreeds~
              </ul>
            </li>
        """
    )
    ~~
      { onClick:
          D.OnClick !:= do
            props.setPage $ BreedDetailsPage props.breed.name

      , name: text_ props.breed.name
      , subBreeds:
          props.breed.subBreeds # foldMap (\name -> (pursx :: _ "<li>~name~</li>") ~~ { name: text_ name })

      }
-}
