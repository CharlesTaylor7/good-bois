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
   
