{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|Functions utilizing Kleisli Arrows to merge, pipe & split input.
-}
module Control.Arrow.Utils (

   -- * Lifting & composing functions
   liftP,
   (>>>),
   (<<<),
   idP,

   -- * Splitting inputs
   (<>),
   (<>*),

   -- ** Convenience functions
   copy,
   flattenP,
   switch,

   -- * Merging outputs
   (><),

   -- * Guards
   guard,
   (??),
   failIf,
   checkThat,

   -- ** Guard with messages
   failIfEither,

   -- * Type synonyms for common pieces
   Pipe,

   -- ** Splitters
   Splitter,
   ManySplitter,

   -- ** Mergers
   Merger,
   ManyMerger
   ) where

import Control.Monad(MonadPlus(..), liftM)
import Control.Monad.Error ((>=>))
import Control.Arrow
import Control.Monad.Trans.Either

-- |A basic (monadic) function.
type Pipe m a b = Kleisli m a b

-- |A function with two outputs.
type Splitter m a b c = Kleisli m a (b,c)
-- |A function with three outputs.
type ManySplitter m a b = Kleisli m a [b]

-- |A function with two inputs.
type Merger m a b c = Kleisli m (a,b) c
-- |A function with three inputs.
-- |A function with a list of inputs.
type ManyMerger m a b = Kleisli m [a] b

-- |Wraps the return value of a function into a monad.
liftP :: Monad m => (a -> b) -> Pipe m a b
liftP = Kleisli . (return .)

-- |The identity pipe.
idP :: Monad m => Pipe m a a
idP = liftP id

-- |Splits an input into two parts and gives each to a handler.
(<>) :: Monad m
     => Splitter m a b c
     -> (Pipe m b d, Pipe m c e)
     -> Splitter m a d e
(<>) f (g,h) = f >>> first g >>> second h

-- |Splits an input into a list and gives each list element to a handler.
(<>*) :: Monad m
      => ManySplitter m a b
      -> Pipe m b c
      -> ManySplitter m a c
(<>*) s p = Kleisli (runKleisli s >=> mapM (runKleisli p))

-- |Takes a splitter and merges its outputs.
(><) :: Monad m
     => Splitter m a b c
     -> Merger m b c d
     -> Pipe m a d
(><) = (>>>)

-- |Applies a pipe if a certain condition is met.
--  If the condition is false, the identity pipe (id) is returned.
guard :: Monad m
      => (a -> Bool)
      -> Pipe m a a
      -> Pipe m a a
guard f p = Kleisli $ \x -> if f x then return x else runKleisli p x

-- |Synonym for 'guard'.
(??) :: Monad m
      => (a -> Bool)
      -> Pipe m a a
      -> Pipe m a a
(??) = guard

-- |Induces a global failure that causes everything
--  down the line to fail if a certain condition is met.
failIf :: MonadPlus m => (a -> Bool) -> Pipe m a a -> Pipe m a a
failIf f p = Kleisli $ \x -> if f x then mzero else runKleisli p x

-- |A variant of @failIf@ which inserts an error message in case of
--  failure.
failIfEither :: Monad m =>
             (a -> Bool)
             -> String
             -> Pipe (EitherT String m) a a
             -> Pipe (EitherT String m) a a
failIfEither f err p = Kleisli $
  \x -> if f x then EitherT $ return $ Left err else runKleisli p x

-- |Inverse of 'failIf': only proceeds if a certain condition is
--  met and induces a global failure otherwise.
checkThat :: MonadPlus m => (a -> Bool) -> Pipe m a a -> Pipe m a a
checkThat f = failIf (not . f)

copy :: Monad m => Splitter m a a a
copy =  liftP (id &&& id)

-- |Flattens the result of a Splitter 'a -> m (b, m c)' into
--  'a -> m (b, c)'. This is useful is 'm c' is some monadic
--  auxiliary information that was previously added.
flattenP :: (Monad m)
         => Pipe m (b, m c) (b,c)
flattenP = Kleisli $ \(x,y) -> liftM (x,) y

-- |Switches the outputs of a splitter.
switch :: Monad m => Pipe m (a,b) (b,a)
switch = liftP (\(x,y) -> (y,x))
