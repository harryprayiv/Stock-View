module Main where

import Render (app)
import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Starting main"
  app