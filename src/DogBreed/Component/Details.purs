module DogBreed.Component.Details where
{-
import Prelude

import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Tuple.Nested ((/\))
import Deku.Common (Nut, pursx, text_, (!:=), (<#~>), (~~))
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState)
import DogBreed.Types (Breed, Page(..))
import Effect (Effect)
import Effect.Console as Console
import Record as Record
import Type.Proxy (Proxy(..))

defaultBreed :: Breed
defaultBreed = { name: "Hound", subBreeds: [ "Basset", "Daschan" ] }

component ::
  forall rest.
  { setPage :: Page -> Effect Unit
  , breed :: String
  | rest
  } ->
  Nut

component props = Deku.do
  ( pursx ::
      _
        """
        <div class="flex flex-col items-center justify-center">
          <a 
          class="flex items-center p-2 cursor-pointer underline decoration-blue-400 text-sky-500"
          ~breadcrumbAttrs~
          >
            Back to Breeds
          </a>
          <h2 class="text-xl font-semibold">Dog Breed Images</h2>
          <ul class="pl-4 list-disc">
          </ul>
        </div>
        """
  ) ~~
    { breadcrumbAttrs: D.OnClick !:= do
        props.setPage BreedListPage
    }

-}
