module Lambadas where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)


------
-- Types

data ThingsOfInterest
  = Sun
  | Saturn

derive instance eqThingOfInterest :: Eq ThingsOfInterest

data Type
  = Atomic
  | Arrow Type Type

derive instance eqType :: Eq Type

data Term
  = Var Int
  | Application Term Term
  | Lambda Term
  | Annotation Term Type
  | ThingTerm ThingsOfInterest

derive instance eqTerm :: Eq Term

data Value
  = ValClosure Closure
  | Stuck StuckOnAVar
  | ValThing ThingsOfInterest

data StuckOnAVar -- neutral value
  = StuckVar Int -- x_n
  | StuckApplication StuckOnAVar Value

data Closure = Closure EvaluationEnvironment Term

data EvaluationEnvironment
  = Nil
  | Cons Value EvaluationEnvironment

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

instance showInterestingThings :: Show ThingsOfInterest where
  show thing = case thing of
    Sun -> "\"Sun\""
    Saturn -> "\"Saturn\""

instance showType :: Show Type where
  show tau = case tau of
    Atomic -> "Atomic"
    Arrow source target -> show source <> " -> " <> show target

instance showTerm :: Show Term where
  show term = case term of
    Var int -> show int
    Application termHead termArg -> "(" <> show termHead <> ")(" <> show termArg <> ")"
    Lambda term' -> "λ." <> show term'
    Annotation term' tau -> show term' <> ": " <> show tau
    ThingTerm thing -> show thing

instance showEnv :: Show EvaluationEnvironment where
  show env = case env of
    Nil -> "[]"
    Cons val moreEnv -> show moreEnv <> ", " <> show val

instance showClos :: Show Closure where
  show (Closure env term) = "Closure: [" <> show env <> "] " <> show term

instance showValue :: Show Value where
  show val = case val of
    ValClosure clos -> "Val " <> show clos
    Stuck stuckOnAVar -> "STUUUUUUUCK"
    ValThing thing -> show thing


------
-- Evaluation

-- apply synonymous with instantiate wtf?
appClosure :: Closure -> Value -> Value
appClosure (Closure env termBody) argument =
  let
    newEnv = Cons argument env
  in
    evaluate newEnv termBody

appTerm :: Value -> Value -> Value
appTerm valHead valArg =
  case valHead of
    ValClosure clos ->
      appClosure clos valArg
    Stuck stuckOnAVar ->
      Stuck (StuckApplication stuckOnAVar valArg)
    ValThing thing ->
      unsafeCoerce (unsafePerformEffect (Console.log "that's a thing not a function!"))

evaluate :: EvaluationEnvironment -> Term -> Value
evaluate env term = case term of
  Var index ->
    envLookup env index

  Application termHead termArgument ->
    let
      valHead = evaluate env termHead
      valArg = evaluate env termArgument
    in
      appTerm valHead valArg

  Lambda termBody ->
    ValClosure (Closure env termBody)

  Annotation termA tau ->
    evaluate env termA

  ThingTerm thing -> ValThing thing


------
-- Type-checking

data Context
  = EmptyContext
  | GammaCons Type Context

contextLookup :: Context -> Int -> Maybe Type
contextLookup context pos =
  case context of
    EmptyContext ->
      Nothing
    GammaCons t nextContext ->
      if pos == 0
      then
        Just t
      else
        contextLookup nextContext (pos - 1)

synth :: Context -> Term -> Maybe Type
synth context term =
  case term of
    Var int ->
      contextLookup context int
    Application termHead termArg ->
      case synth context termHead of
        Just (Arrow paramType bodyType) ->
          if check context termArg paramType
          then Just bodyType
          else Nothing
        _ ->
          Nothing
    Lambda body ->
      Nothing
    Annotation term' t ->
      if check context term' t
      then Just t
      else Nothing
    ThingTerm something ->
      Just Atomic

check :: Context -> Term -> Type -> Boolean
check context term t =
  case term, t of
    Lambda body, Arrow paramType bodyType ->
      let
        bodyContext = GammaCons paramType context
      in
        check bodyContext body bodyType
    _, _ ->
      case synth context term of
        Just t' -> t == t'
        Nothing -> false
