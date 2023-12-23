module DogCeo.Api.Utils
  ( Result(..)
  , attempt
  ) where

import Prelude

import Data.Either (either)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (Error)

data Result a
  = Loading
  | Error Error
  | Success a

attempt :: forall a. Aff a -> Aff (Result a)
attempt aff = Aff.attempt aff <#> either Error Success
