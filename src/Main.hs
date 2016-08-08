{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor.Identity (Identity)
import Data.Monoid ((<>))
import Testing.Cucumber (cucumber)
import Testing.Cucumber.Types (Mapping(..), Report)
import Testing.Cucumber.Mappings (mapp)
import Text.Read (readMaybe)

main = cucumber cucinoMappings

cucinoMappings :: [Mapping Identity]
cucinoMappings =
  [ mapp Given "^a calculator$" $ \[] () () -> Right ()
  , mapp When  "^these numbers are summed together:$" $ \[] (s :: String) () ->
      maybe (Left "Should be one number per line") Right $
        sum <$> traverse readMaybe (lines s) :: Report Int
  , mapp When  "^these numbers are multiplied:$" $ \[] (s :: [[String]]) () ->
      maybe (Left "Should be one number per cell") Right $
        foldl (*) 1 <$> traverse readMaybe (concat s) :: Report Int
  , mapp Then  "^the result is ([0-9]+)$" $ \[x] () n ->
      case (n ==) <$> (readMaybe x :: Maybe Int) of
        Just True  -> Right ()
        Just False -> Left $ "Expected " <> show n <> " to be " <> x
        Nothing    -> Left "Not a number!"
  ]
