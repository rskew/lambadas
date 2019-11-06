module Lambadas where

import Prelude
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

data ThingsOfInterest
  = Sun
  | Saturn

derive instance eqThingOfInterest :: Eq ThingsOfInterest

instance showInterestingThings :: Show ThingsOfInterest where
  show thing = case thing of
    Sun -> "\"Sun\""
    Saturn -> "\"Saturn\""

data Type
  = Atomic
  | Arrow Type Type

derive instance eqType :: Eq Type

instance showType :: Show Type where
  show tau = case tau of
    Atomic -> "Atomic"
    Arrow source target -> show source <> " -> " <> show target

data Term
  = Var Int
  | Application Term Term
  | Lambda Term
  | Annotation Term Type
  | ThingTerm ThingsOfInterest

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show term = case term of
    Var int -> show int
    Application termHead termArg -> "(" <> show termHead <> ")(" <> show termArg <> ")"
    Lambda term' -> "λ." <> show term'
    Annotation term' tau -> show term' <> ": " <> show tau
    ThingTerm thing -> show thing

data EvaluationEnvironment
  = Nil
  | Cons Value EvaluationEnvironment

instance showEnv :: Show EvaluationEnvironment where
  show env = case env of
    Nil -> "[]"
    Cons val moreEnv -> show moreEnv <> ", " <> show val

envLength :: EvaluationEnvironment -> Int
envLength env = case env of
  Nil -> 0
  Cons _ restEnv -> 1 + envLength restEnv

envLookup :: EvaluationEnvironment -> Int -> Value
envLookup env index = case env of
  Nil ->
    unsafeCoerce (unsafePerformEffect (Console.log "oh geez not good!"))
  Cons val restEnv ->
    if index == 0
    then val
    else envLookup restEnv (index - 1)

data Closure = Closure EvaluationEnvironment Term

instance showClos :: Show Closure where
  show (Closure env term) = "Closure: [" <> show env <> "] " <> show term

data Value
  = ValClosure Closure
  | Stuck StuckOnAVar
  | ValThing ThingsOfInterest

instance showValue :: Show Value where
  show val = case val of
    ValClosure clos -> "Val " <> show clos
    Stuck stuckOnAVar -> "STUUUUUUUCK"
    ValThing thing -> show thing

data StuckOnAVar
  = StuckVar Int
  | StuckApplication StuckOnAVar Term

instantiateVar :: EvaluationEnvironment -> Value
instantiateVar env =
  Stuck (StuckVar (envLength env))

appClosure :: Closure -> Value -> Value
appClosure (Closure env termBody) argument =
  let
    newEnv = Cons argument env
  in
    evaluate newEnv termBody

evaluate :: EvaluationEnvironment -> Term -> Value
evaluate env term = case term of
  Var index ->
    envLookup env index

  Application termHead termArgument ->
    case evaluate env termHead of
      ValClosure clos ->
        let
          valArg = evaluate env termArgument
        in
          appClosure clos valArg
      Stuck stuckOnAVar ->
        Stuck (StuckApplication stuckOnAVar termArgument)
      ValThing thing ->
        unsafeCoerce (unsafePerformEffect (Console.log "that's a thing not a function!"))

  Lambda termBody ->
    ValClosure (Closure env termBody)

  Annotation termA tau ->
    evaluate env termA

  ThingTerm thing -> ValThing thing
