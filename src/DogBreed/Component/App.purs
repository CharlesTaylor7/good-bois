module DogBreed.Component.App where
{-

import Prelude
import Utils

import Data.Foldable (foldMap, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Deku.Common (Nut, pursx, text_, useAff, (!:=), (<#~>), (~~))
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState)
import DogBreed.Component.Details as DetailsPage
import DogBreed.Component.List as ListPage
import DogBreed.Types (ApiResult(..), Breed, Page(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import FRP.Event as FRP
import Foreign.Object as FO
import Record as Record
import Type.Proxy (Proxy(..))


component :: Nut
component = Deku.do
  setPage /\ page <- useState BreedListPage
  setBreeds /\ breeds <- useState (Loading :: ApiResult (Array Breed))

  useAff (pure unit) $ \_ -> do
  -- TODO:
  -- image cache

  let props = { breeds: monitor "breeds" breeds, setPage }
  page <#~> case _ of
    BreedListPage ->
      ListPage.component props
    BreedDetailsPage breed ->
      DetailsPage.component $ Record.insert (Proxy :: _ "breed") breed props
      -}
