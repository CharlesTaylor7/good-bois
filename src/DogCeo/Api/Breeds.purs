module DogCeo.Api.Breeds
  ( fetch
  ) where

import Prelude

import Data.Tuple.Nested ((/\))
import DogCeo.Types (ApiResult(..), Breed)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch as Fetch
import Fetch.Argonaut.Json as Json
import Foreign.Object as FO

type DogBreedResponse =
  { message :: FO.Object (Array String)
  }

fetch :: forall monad. MonadAff monad => monad (ApiResult (Array Breed))
fetch = liftAff $ do
  { json } <- Fetch.fetch "https://dog.ceo/api/breeds/list/all" {}
  { message } :: DogBreedResponse <- Json.fromJson json

  pure $ Success $ (FO.toAscUnfoldable message :: Array _)
    <#> \(name /\ subBreeds) ->
      { name, subBreeds }

