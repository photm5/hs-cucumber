# Cucumber for Haskell

This is an attempt to build a cucumber implementation for Haskell in Haskell.
There are multiple unfinished ones already, so I thought I’d add one to the
list.  Here’s a small comparison showing why the other attempts have flaws (If I
missed an attempt, please tell me by opening an issue!):

1. [chuchu][1]:  I didn’t look at it thoroughly, but the example tells me that
   it doesn’t allow switching the type of the state between steps.  One could
   argue that it doesn’t really matter, since one can just create a huge sum
   type, but then it doesn’t allow mixing and matching step definitions from
   libraries and own code.  (Have a look at aruba if you don’t know what I’m
   talking about.)

2. [cucumber-haskell][2]:  It uses Template Haskell.  As far as I can tell, the
  idea is that test terms (Haskell code) are generated from the feature files.
  (Composing the step definitions at runtime doesn’t work except by using
  `Dynamic`, since they have different types.)  This comes with some
  disatvantages, the main one being that if the test term doesn’t compile and
  run, the other scenarios (or at least steps) will probably not be run at all.

3. [haskell-cucumber][3]:  Since I want to give the right credit, according to
  @sol and the commit logs, it was written by @sakari, however his copy of the
  repository doesn’t exist anymore.  As far as I can tell, in order to keep
  values between the execution of steps, one has to save them into IO variables,
  which is not nice.

[1]: https://github.com/marcotmarcot/chuchu
[2]: https://github.com/sol/cucumber-haskell
[3]: https://github.com/Erkan-Yilmaz/haskell-cucumber

So, now that I’ve complained about the others, let’s talk about the flaws of my
attempt.  We just saw the same problem from three different perspectives:
Haskell isn’t dynamically typed, and running as many passing steps as possible
asks for deferring type errors to runtime.  I propose to approach the problem
from a different angle:  Embracing the need of dynamic typing to (kind-of)
elegantly solve the problem.

This means I’m using `Dynamic` and `Typeable` (which are not the nicest of
things, but arguably necessary in this case) in order to allow the use of
different types and handling mismatches at run time.  Let’s look at my
implementation.  The following types are most important to use the library:

```haskell
-- Other monads beside Data.Functor.Identity can be supported with ease.
cucumber :: [Mapping Identity] -> IO ()
mapp :: (Typeable a, Typeable b, MultilineArgLike l)
     => MappingConstructor Identity
     -> String -- ^ Regular expression
     -> ([String]     -- ^ Group matches of the regular expression
         -> l         -- ^ A MultilineArgLike, see below
         -> a         -- ^ The state before executing the step
         -> Report b) -- ^ A Report on the state after execution, see below
     -> Mapping Identity
Given :: MappingConstructor m
When  :: MappingConstructor m
Then  :: MappingConstructor m
instance MultilineArgLike ()           -- Don’t expect a multiline argument
instance MultilineArgLike MultilineArg -- Expect any multiline argument
instance MultilineArgLike [[String]]   -- Expect a table
instance MultilineArgLike String       -- Expect a multiline string
-- This is (among everything else) hopefully subject to change, since cucumber
-- supports lots of different types of data in error reports, including images
-- and videos:
type Report newState = Either String newState
```

Here’s the implementation I [bootstrapped][4]:

```haskell
-- Imagine all the imports here

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
```

[4]: https://github.com/cucumber/cucumber-bootstrap

If the types of state between two staps don’t match, a type error is printed at
runtime.  All the other steps (except those that are skipped) are still run.

I currently use [abacate][5] to parse gherkin files, because it is on hackage.
[Cucumber-haskell][2] recommends the use of [haskell-gherkin][6], which I hope
would make pretty-printing steps in color while executing them very easy.  That
would be a good reason to switch to it.

[5]: http://github.com/marcotmarcot/abacate
[6]: https://github.com/sakari/haskell-gherkin

What if there’s multiple values we want to store between execution of steps?
That’s a valid question, especially in case we want to combine multiple things
that are given etc.  My idea is that we could use `Dynamic` to build a
heterogenous association list akin to this:

```haskell
type State a = [(a, Dynamic)]

lookupDyn :: (Eq a, Typeable b) => a -> State a -> Maybe b
lookupDyn k = lookup k >=> fromDynamic

putDyn :: (Eq a, Typeable b) => a -> b -> State a -> State a
putDyn k v s = case lookupDyn k s of
  Nothing -> (k, toDyn v) : s
  Just v' -> (k, toDyn $ v `asTypeOf` v') : filter ((/= k) . fst) s
```

Everything then boils down to choosing good identifiers.  E.g. if we want to
avoid sum types, `String` might be a valid option.

I’m not sure at all how much time I will spend on this, so please take all the
good ideas and do something great!  I even chose BSD instead of AGPL in case you
don’t like copyleft as much as I do.
