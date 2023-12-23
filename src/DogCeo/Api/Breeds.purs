module DogCeo.Api.Breeds
  ( fetch
  ) where

import Prelude

import Data.Tuple.Nested ((/\))
import DogCeo.Api.Utils as Api
import DogCeo.Api.Utils as Api
import DogCeo.Types (BreedGroup)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch as Fetch
import Fetch.Argonaut.Json as Json
import Foreign.Object as FO

type DogBreedResponse =
  { message :: FO.Object (Array String)
  }

fetch :: forall monad. MonadAff monad => monad (Api.Result (Array BreedGroup))
fetch = liftAff $ Api.attempt $ do
  { json } <- Fetch.fetch "https://dog.ceo/api/breeds/list/all" {}
  { message } :: DogBreedResponse <- Json.fromJson json

  pure $ (FO.toAscUnfoldable message :: Array _)
    <#> \(name /\ subBreeds) ->
      { name, subBreeds }
