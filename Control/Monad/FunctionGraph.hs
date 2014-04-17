{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|Functions to create directed graphs of functions through which
   data can be sent, in an ordered manner; quite similar to Kleisli arrows.

   Encourages, in essence, point-free programming and transformation-centric
   view of programs.
-}
module Control.Monad.FunctionGraph (

   -- * Lifting & composing functions
   liftP,
   (>=>),
   (<=<),
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

   -- * Executing functions in parallel
   mapPar,
   (=|>),

   -- ** History-preserving lists
   liftPar,
   liftPar',

   -- ** History-erasing lifts
   liftParErase,
   liftParErase',

   -- * Guards
   guard,
   (??),
   failIf,
   checkThat,

   -- ** Guard with messages
   failIfEither,

   -- * Type synonyms for common pieces
   Pipe,
   ParPipe,

   -- ** Splitters
   Splitter,
   ManySplitter,

   -- ** Mergers
   Merger,
   ManyMerger
   ) where

import Control.Monad(MonadPlus(..), liftM)
import Control.Monad.Error ((>=>), (<=<))
import Control.Arrow
import Control.Monad.MonadFilter
import Control.Monad.Trans.Either

-- |A basic (monadic) function.
type Pipe m a b = a -> m b

-- |A function with two outputs.
type Splitter m a b c = a -> m (b,c)
-- |A function with three outputs.
type ManySplitter m a b = a -> m [b]

-- |A function with two inputs.
type Merger m a b c = (a,b) -> m c
-- |A function with three inputs.
-- |A function with a list of inputs.
type ManyMerger m a b = [a] -> m b

-- |A parallel execution which has both local and
--  global state (the computation may fail in certain
--  places, or as a whole).
--  If the input values are unique, 
--  the computation is injective ---
--  the result is a list of pairs (a,b), where a is the original
--  input and b the value to which it was mapped.
type ParPipe m a b c = [(a,b)] -> m [(a, m c)]

-- |Wraps the return value of a function into a monad.
liftP :: Monad m => (a -> b) -> Pipe m a b
liftP = (return .)

-- |The identity pipe.
idP :: Monad m => Pipe m a a
idP = liftP id

-- |Splits an input into two parts and gives each to a handler.
(<>) :: Monad m
     => Splitter m a b c
     -> (Pipe m b d, Pipe m c e)
     -> Splitter m a d e
(<>) f (g,h) x = do (y,z) <- f x
                    y' <- g y
                    z' <- h z
                    return (y',z')

-- |Splits an input into a list and gives each list element to a handler.
(<>*) :: Monad m
      => ManySplitter m a b
      -> Pipe m b c
      -> ManySplitter m a c
(<>*) f g x = f x >>= mapM g

-- |Takes a splitter and merges its outputs.
(><) :: Monad m
     => Splitter m a b c
     -> Merger m b c d
     -> Pipe m a d
(><) = (>=>)

-- |Lifts a pipe to a parPipe - applies the pipe to all
--  elements of a list in parallel.
mapPar :: Monad m
       => Pipe m b c
       -> ParPipe m a b c
mapPar p = return . map (second p)

-- |Lifts a function 'f : [b] -> [(b,c)]'
--  (where the result consists of input-output-pairs) to a parPipe.
--  'f' has to preserve the order of the elements of its input-list,
--  i.e xs !! i must be mapped 1:1 to a pair f xs !! i.
liftPar :: (Monad m)
        => ([b] -> [(b,c)])
        -> ParPipe m a b c
liftPar f xs = return
               . zipWith (\(a,_) (_,c) -> (a,c)) xs
               . map (second return)
               . f
               . snd
               . unzip
               $ xs

-- |Lifts a function 'f: [a] -> [b]'' to a parPipe.
--  'f' has to preserve the order of the elements in its input-list,
--  i.e. xs !! i must be mapped 1:1 to f xs !! i.
liftPar' :: (Monad m)
         => ([b] -> [c])
         -> ParPipe m a b c
liftPar' f = liftPar $ uncurry zip . (id &&& f)

-- |Lifts a function 'f : [a] -> [(a,c)]'
--  (where the result consists of input-output-pairs) to a parPipe.
--  'f' has to preserve the order of the elements of its input-list,
--  i.e xs !! i must be mapped 1:1 to a pair f xs !! i.
--
--  Whereas @liftPar@, for each input pair '(A,B)', a pipe made with
--  @liftPar@ creates an output pair '(A,m C)', liftParErase creates
--  an output pair '(B,m C)'. Since 'A' corresponds to the input
--  of a previous pipe and 'B' to its ouput, liftParErase \'erases
--  history' by outputting 'B' instead of 'A'.
liftParErase :: Monad m
             => ([a] -> [(a,b)])
             -> ParPipe m a a b
liftParErase f xs = return . map (second return) . f . snd . unzip $ xs

-- |Lifts a function 'f: [a] -> [b]'' to a parPipe.
--  'f' has to preserve the order of the elements in its input-list,
--  i.e. xs !! i must be mapped 1:1 to f xs !! i.
-- 
-- History-erasing version of @liftPar'@.
liftParErase' :: (Monad m)
         => ([a] -> [b])
         -> ParPipe m a a b
liftParErase' f = liftParErase $ uncurry zip . (id &&& f)

-- |Concatenates two parPipes.
--  If the first fails globally, the concatenated parPipe
--  fails globally too. If not, the first parPipe is
--  applied, all local failures are removed, and after that,
--  the second parPipe is applied.
(=|>) :: (MonadFilter m)
      => ParPipe m a b c
      -> ParPipe m a c d
      -> ParPipe m a b d
(=|>) p q x = do px <- p x
                 (successes, failures) <- partitionM px
                 qx <- q successes
                 return $! qx ++ failures
  where 
   -- folds a list and partitions it based on whether the second
   -- component of each tuple is mzero. The first list of the result
   -- is that of the non-zeros and the second contains the zeros.
   partitionM :: MonadFilter m => [(a, m b)] -> m ([(a, b)], [(a, m c)])
   partitionM [] = return ([], [])
   partitionM ((a,b):xs) =
      do b' <- ismzero' b
         (ys,ns) <- partitionM xs
         -- "b >>= const mzero" is there to make the "m c" in the return
         -- type possible. If we just wrote "b", the return type would
         -- be "m b"; if
         -- we wrote "mzero", that'd fit, but in the case of Either,
         -- the error message contained in b would be discarded, since
         -- mzero = Left noMsg. b >>= const mzero retains b as is,
         -- but satisfies the type checker.
         case b' of Nothing -> return (ys,(a,b >>= const mzero):ns)
                    Just b'' -> return ((a,b''):ys,ns)

-- |Applies a pipe if a certain condition is met.
--  If the condition is false, the identity pipe (id) is returned.
guard :: Monad m
      => (a -> Bool)
      -> Pipe m a a
      -> Pipe m a a
guard f p x = if f x then return x else p x

-- |Synonym for 'guard'.
(??) :: Monad m
      => (a -> Bool)
      -> Pipe m a a
      -> Pipe m a a
(??) = guard

-- |Induces a global failure that causes everything
--  down the line to fail if a certain condition is met.
failIf :: MonadPlus m => (a -> Bool) -> Pipe m a a -> Pipe m a a
failIf f p x = if f x then mzero else p x

-- |A variant of @failIf@ which inserts an error message in case of
--  failure.
failIfEither :: Monad m =>
             (a -> Bool)
             -> String
             -> Pipe (EitherT String m) a a
             -> Pipe (EitherT String m) a a
failIfEither f err p x = if f x then EitherT $ return $ Left err else p x

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
flattenP (x,y) = liftM (x,) y

-- |Switches the outputs of a splitter.
switch :: Monad m => Splitter m a b c -> Splitter m a c b
switch = (<=<) $ liftP (\(x,y) -> (y,x))
