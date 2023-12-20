module DogBreed.Component.List where

import Prelude

import Data.Foldable (foldMap)
import Deku.Common (Nut, pursx, text_, useState, (!:=), (<#~>), (~~))
import Deku.DOM as D
import Deku.Do as Deku
import DogBreed.Types (Breed, Page(..))
import Effect (Effect)
import Effect.Console as Console

defaultBreed :: Breed
defaultBreed = { name: "Hound", subBreeds: [ "Basset", "Daschan" ] }

component ::
  forall rest.
  { setPage :: Page -> Effect Unit
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
          D.OnClick !:= do
            Console.log $ "Hello " <> breed.name

      , subBreeds:
          breed.subBreeds # foldMap (\name -> (pursx :: _ "<li>~name~</li>") ~~ { name: text_ name })
      }

