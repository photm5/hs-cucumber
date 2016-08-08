{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testing.Cucumber.Mappings where

import Data.Dynamic (Dynamic, Typeable, dynTypeRep, fromDynamic, toDyn, typeOf)
import Data.Functor.Identity (Identity(..))
import Data.Monoid ((<>))
import Language.Abacate (MultilineArg)
import Testing.Cucumber.Types (Handler, Handler', Keyword(..), Mapping(..),
    MappingConstructor, MultilineArgLike, Report, fromMultilineArg)
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~~), getAllTextSubmatches)

mappHM :: (Monad m, Typeable a, Typeable b)
       => MappingConstructor m
       -> String         -- ^ The regular expression to match against
       -> Handler a b m  -- ^ The code to be executed if it matches
       -> Mapping m      -- ^ A Mapping to pass to cucumber
mappHM c e h = c e h'
  where h' gs m d = case fromDynamic d of
          Nothing -> return . Left $ "Could not match " <> show (dynTypeRep d)
                                  <> " with " <> show (typeOf hypothetArg)
          Just x  -> fmap toDyn <$> h gs m x
        hypothetArg = undefined $ h undefined undefined hypothetArg

mappH :: (Typeable a, Typeable b)
      => MappingConstructor Identity
      -> String
      -> ([String] -> Maybe MultilineArg -> a -> Report b)
      -> Mapping Identity
mappH c e h = mappHM c e $ toIdentHandler h
  where toIdentHandler h gs m s = Identity $ h gs m s

mappM :: (Monad m, Typeable a, Typeable b, MultilineArgLike l)
      => MappingConstructor m
      -> String
      -> ([String] -> l -> a -> m (Report b))
      -> Mapping m
mappM c e h' = mappHM c e h
  where h gs m d = case fromMultilineArg m of
          Nothing -> return $ Left "Multiline Argument mismatch"
          Just x  -> h' gs x d

mapp :: (Typeable a, Typeable b, MultilineArgLike l)
     => MappingConstructor Identity
     -> String
     -> ([String] -> l -> a -> Report b)
     -> Mapping Identity
mapp c e h = mappM c e $ toIdentHandler h
  where toIdentHandler h gs m s = Identity $ h gs m s

exampleMappings :: [Mapping Identity]
exampleMappings =
  [ mapp Given "the number ([0-9]*)" $ \[x] () () ->
      maybe (Left "Not a number!") Right (readMaybe x :: Maybe Int)
  , mapp When  "I add ([0-9]+) to it" $ \[x] () (n :: Int) ->
      maybe (Left "Not a number!") Right $ (n +) <$> readMaybe x
  , mapp Then "the result is ([0-9]+)" $ \[x] () (n :: Int) ->
      maybe (Left "Not a number!") Right $ (n ==) <$> readMaybe x
  ]

matchM :: (Monad m)
       => Mapping m
       -> Keyword
       -> String
       -> Maybe MultilineArg
       -> Maybe (Dynamic -> m (Report Dynamic))
matchM (Given e h) KwGiven s = matchM' e h s
matchM (When  e h) KwWhen  s = matchM' e h s
matchM (Then  e h) KwThen  s = matchM' e h s
matchM _ _ _ = const Nothing

matchM' :: (Monad m)
        => String
        -> Handler' m
        -> String
        -> Maybe MultilineArg
        -> Maybe (Dynamic -> m (Report Dynamic))
matchM' e h s m = h <$> submatches <*> Just m
  where submatches :: Maybe [String]
        submatches = tail . getAllTextSubmatches <$> s =~~ e

match :: Mapping Identity -- ^ A Mapping
      -> Keyword          -- ^ The Keyword of a step
      -> String           -- ^ The text of a step
      -> Maybe MultilineArg
      -> Maybe (Dynamic -> Report Dynamic) -- ^ If mapping matches, a conversion
                                           --   on the state
                                --   state
match m k s ml = (runIdentity .) <$> matchM m k s ml
