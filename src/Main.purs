module Main where

import Prelude

import Deku.Toplevel (runInBody)
import DogBreed.Component.App as App
import Effect (Effect)

main :: Effect Unit
main = runInBody App.component
