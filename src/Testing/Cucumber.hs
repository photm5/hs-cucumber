{-# LANGUAGE ScopedTypeVariables #-}

module Testing.Cucumber where

import Control.Monad (forM_, liftM2)
import Data.Char (toLower)
import Data.Dynamic (Dynamic, toDyn)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (unpack)
import qualified Language.Abacate as A (Step, StepKeyword(..))
import Language.Abacate (Feature, FeatureElement(..), MultilineArg, bsName,
    bsSteps, fBackground, fFeatureElements, parseFile, scBasicScenario, stBody,
    stMultilineArg, stStepKeyword)
import System.Environment (getArgs)
import Testing.Cucumber.Types (Keyword(..), Mapping(..), Report, Results(..),
    Scenario(..), ScenarioResult(..), Step(..), StepResult(..), isFailed,
    mergeRess)
import Testing.Cucumber.Mappings (mapp, match)
import Text.Read (readMaybe)

import Debug.Trace (traceShow)

cucumber :: [Mapping Identity] -> IO ()
cucumber ms = do
  [filename] <- getArgs
  res <- parseFile filename
  case res of
    Left err -> do
      putStrLn "Parse failed, error:"
      print err
    Right parsed -> do
      putResults =<< cucumberOnScens ms (makeScens parsed)

makeScens :: Feature -> [Scenario]
makeScens f = map (addB . fromBS . fromFES) $ fFeatureElements f
  where fromFES (FES x) = scBasicScenario $ x
        addB (Scenario n s) = Scenario n (maybe [] (scenarioSteps . fromBS)
                                                   (fBackground f) <> s)
        fromBS bs = Scenario (unpack $ bsName bs) (mkSteps $ bsSteps bs)
        mkSteps :: [A.Step] -> [Step]
        mkSteps = reverse . foldl carryRealKw []
          where carryRealKw :: [Step] -> A.Step -> [Step]
                carryRealKw xs s = Step (kw s) (unpack $ stBody s)
                                        (stMultilineArg s) : xs
                  where kw = overrideKw (stepKeyword $ head xs) . stStepKeyword
                overrideKw :: Keyword -> A.StepKeyword -> Keyword
                overrideKw _ A.Given = KwGiven
                overrideKw _ A.When  = KwWhen
                overrideKw _ A.Then  = KwThen
                overrideKw x _     = x

putResults :: Results -> IO ()
putResults rs = do
  putStrLn $ "Results: Scenarios - " <> show nScen
                     <> ", Steps - " <> show nStep
  forM_ rs $ \r -> do
    putStrLn $ (scenarioName $ scenario r)
            <> ": " <> showRes (foldl1 mergeRess $ stepResults r)
  where nScen = length rs
        nStep = sum $ map (length . stepResults) rs
        showRes (Failed x) = "failed: " <> x
        showRes x = map toLower $ show x

cucumberOnScens :: [Mapping Identity] -> [Scenario] -> IO Results
cucumberOnScens ms ss = mapM (cucumberOnScen ms) ss

cucumberOnScen :: [Mapping Identity] -> Scenario -> IO ScenarioResult
cucumberOnScen ms s = return . ScenarioResult s . reverse . fst $
                        foldl bind ([], toDyn ()) (scenarioSteps s)
  where bind :: ([StepResult], Dynamic) -> Step -> ([StepResult], Dynamic)
        bind (rs, state) step
          | any isFailed rs = (Skipped : rs, state)
          | otherwise = case matches step of
            [] -> (Pending : rs, state)
            [f] -> case f state of
              Left err -> (Failed err : rs, state)
              Right state' -> (Passed : rs, state')
            _ -> (Failed "TODO: Error message" : rs, state)
        matches :: Step -> [(Dynamic -> Report Dynamic)]
        matches s = catMaybes $ map (($ stepMultiline s) . ($ stepText s) .
                                     ($ stepKeyword s) . match) ms
