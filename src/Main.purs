module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Lambadas (Context(..), EvaluationEnvironment(..), Term(..), ThingsOfInterest(..), Type(..), Value(..), check, evaluate)


main :: Effect Unit
main =
  let
    environment = Cons (ValThing Sun) Nil
    id = Lambda (Var 0)
    term = Application (Annotation id (Arrow (Arrow Atomic Atomic) (Arrow Atomic Atomic))) id
    evaluatedTerm = evaluate environment term
  in do
    log (show evaluatedTerm)
    logShow $ check EmptyContext id (Arrow Atomic Atomic)
    logShow $ check EmptyContext id (Arrow (Arrow Atomic Atomic) (Arrow Atomic Atomic))
    logShow $ check EmptyContext term (Arrow Atomic Atomic)
