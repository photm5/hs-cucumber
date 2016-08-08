{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testing.Cucumber.Types where

import Data.Dynamic (Dynamic, dynTypeRep, fromDynamic, toDyn, typeOf)
import Data.Functor.Identity (Identity(..))
import Data.Monoid ((<>))
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Language.Abacate (MultilineArg(..), Table)
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~~), getAllTextSubmatches)

data Scenario = Scenario
              { scenarioName :: String
              , scenarioSteps :: [Step]
              }

data Step = Step
          { stepKeyword :: Keyword
          , stepText    :: String
          , stepMultiline :: Maybe MultilineArg
          }

type Results = [ScenarioResult]
data ScenarioResult = ScenarioResult
                    { scenario :: Scenario
                    , stepResults :: [StepResult]
                    }

data StepResult = Passed | Failed String | Skipped | Pending
  deriving (Eq, Show)

isFailed :: StepResult -> Bool
isFailed (Failed _) = True
isFailed _          = False

mergeRess :: StepResult -> StepResult -> StepResult
mergeRess (Failed x) _ = (Failed x)
mergeRess _ (Failed x) = (Failed x)
mergeRess Pending _ = Pending
mergeRess _ Pending = Pending
mergeRess _ x = x

type Report = Either String

type Handler a b m = [String]   -- ^ The groups from the regular expression
                                --   match
                -> Maybe MultilineArg -- ^ Multiline argument from step def
                -> a            -- ^ The state before the handler is run
                -> m (Report b) -- ^ A monadic value to obtain the new state

type Handler' m = Handler Dynamic Dynamic m

data Keyword = KwGiven | KwWhen | KwThen

type MappingConstructor m = (String -> (Handler' m) -> Mapping m)

data Mapping m = Given String (Handler' m)
               | When  String (Handler' m)
               | Then  String (Handler' m)

instance Show (Mapping m) where
  show (Given s _) = "Given " <> s
  show (When  s _) = "When "  <> s
  show (Then  s _) = "Then "  <> s

class MultilineArgLike l where
  fromMultilineArg :: Maybe MultilineArg -> Maybe l

instance MultilineArgLike () where
  fromMultilineArg Nothing = Just ()
  fromMultilineArg _       = Nothing

instance MultilineArgLike MultilineArg where
  fromMultilineArg = id

instance MultilineArgLike [[String]] where
  fromMultilineArg (Just (MAT x)) = Just $ map (map unpack) x
  fromMultilineArg _              = Nothing

instance MultilineArgLike String where
  fromMultilineArg (Just (MAPS x)) = Just $ unpack x
  fromMultilineArg _               = Nothing
