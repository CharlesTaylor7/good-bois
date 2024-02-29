module DogCeo.Routes
  ( Route(..)
  , codec
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol)
import DogCeo.Types (Breed)
import Prim.Row as Row
import Routing.Duplex (RouteDuplex, RouteDuplex')
import Routing.Duplex as R
import Routing.Duplex.Generic as G
import Type.Proxy (Proxy(..))

data Route
  = BreedsRoute
  | ImagesRoute { breed :: Breed, page :: Int }

derive instance Eq Route
derive instance Generic Route _

codec :: RouteDuplex' Route
codec = R.root $ R.prefix "good-bois" $ G.sum
  { "BreedsRoute": G.noArgs
  , "ImagesRoute": R.path "images" $ R.record
      # prop @"page" (R.int $ R.param "page")
      # prop @"breed"
          ( R.record
              # prop @"name" (R.string R.segment)
              # prop @"subBreed" (R.optional R.segment)
          )
  }

prop ::
  forall @sym a b r1 r2 r3 rx.
  IsSymbol sym =>
  Row.Cons sym a rx r1 =>
  Row.Cons sym b r2 r3 =>
  Row.Lacks sym r2 =>
  RouteDuplex a b ->
  RouteDuplex (Record r1) (Record r2) ->
  RouteDuplex (Record r1) (Record r3)
prop = R.prop (Proxy :: _ sym)
