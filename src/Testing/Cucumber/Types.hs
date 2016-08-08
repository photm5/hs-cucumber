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

-- TODO: Inline these types

type MapperM m = forall a b. (Typeable a, Typeable b)
              => String         -- ^ The regular expression to match against
              -> Handler a b m  -- ^ The code to be executed if it matches
              -> Mapping m      -- ^ A Mapping to pass to cucumber
type Mapper = forall a b. (Typeable a, Typeable b)
           => String
           -> ([String] -> Maybe MultilineArg -> a -> Report b)
           -> Mapping Identity

mappHM :: (Monad m) => MappingConstructor m -> MapperM m
mappHM c e h = c e h'
  where h' gs m d = case fromDynamic d of
          Nothing -> return . Left $ "Could not match " <> show (dynTypeRep d)
                                  <> " with " <> show (typeOf hypothetArg)
          Just x  -> fmap toDyn <$> h gs m x
        hypothetArg = undefined $ h undefined undefined hypothetArg

mappH :: MappingConstructor Identity -> Mapper
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
