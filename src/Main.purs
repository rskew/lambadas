module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Lambadas (evaluate, EvaluationEnvironment(..), Value(..), ThingsOfInterest(..), Term(..))

main :: Effect Unit
main =
  let
    environment = Cons (ValThing Sun) Nil
    id = Lambda (Var 0)
    term = Application id id
    evaluatedTerm = evaluate environment term
  in
    log (show evaluatedTerm)
