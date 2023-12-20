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
import DogBreed.Component.Details as DetailsPage
import DogBreed.Component.List as ListPage
import DogBreed.Types (Breed, Page(..))
import Effect (Effect)
import Effect.Console as Console
import Record as Record
import Type.Proxy (Proxy(..))

component :: Nut
component = Deku.do
  setPage /\ page <- useState BreedListPage
  -- TODO:
  -- Breed cache
  -- image cache

  let props = { setPage }
  page <#~> case _ of
    BreedListPage ->
      ListPage.component props
    BreedDetailsPage breed ->
      DetailsPage.component $ Record.insert (Proxy :: _ "breed") breed props

