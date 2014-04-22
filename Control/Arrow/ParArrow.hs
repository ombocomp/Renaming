{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|Introduces ParArrows of type @[(a,b)] -> m [(a,m c)]@ which allow
   the parallel execution of arrows on lists inputs and the maintaining
   of a bijection between inputs and outputs.
-}
module Control.Arrow.ParArrow (


   -- * Executing functions in parallel
   mapPar,
   (=|>),

   -- ** History-preserving lists
   liftPar,
   liftPar',

   -- ** History-erasing lifts
   liftParErase,
   liftParErase',

   -- * Type synonyms
   ParArrow,
   ) where

import Control.Monad(MonadPlus(..))
import Control.Arrow
import Control.Monad.MonadFilter

-- |A parallel execution which has both local and
--  global state (the computation may fail in certain
--  places, or as a whole).
--  If the input values are unique, 
--  the computation is injective ---
--  the result is a list of pairs (a,b), where a is the original
--  input and b the value to which it was mapped.
--
--  This is the one data structure which (to my mind)
--  is not simply an arrow.
type ParArrow m a b c = [(a,b)] -> m [(a, m c)]

-- |Lifts a pipe to a ParArrow - applies the pipe to all
--  elements of a list in parallel.
mapPar :: Monad m
       => Kleisli m b c
       -> ParArrow m a b c
mapPar p = return . map (second $ runKleisli p)

-- |Lifts a function 'f : [b] -> [(b,c)]'
--  (where the result consists of input-output-pairs) to a ParArrow.
--  'f' has to preserve the order of the elements of its input-list,
--  i.e xs !! i must be mapped 1:1 to a pair f xs !! i.
liftPar :: (Monad m)
        => ([b] -> [(b,c)])
        -> ParArrow m a b c
liftPar f xs = return
               . zipWith (\(a,_) (_,c) -> (a,c)) xs
               . map (second return)
               . f
               . snd
               . unzip
               $ xs

-- |Lifts a function 'f: [a] -> [b]'' to a ParArrow.
--  'f' has to preserve the order of the elements in its input-list,
--  i.e. xs !! i must be mapped 1:1 to f xs !! i.
liftPar' :: (Monad m)
         => ([b] -> [c])
         -> ParArrow m a b c
liftPar' f = liftPar $ uncurry zip . (id &&& f)

-- |Lifts a function 'f : [a] -> [(a,c)]'
--  (where the result consists of input-output-pairs) to a ParArrow.
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
             -> ParArrow m a a b
liftParErase f xs = return . map (second return) . f . snd . unzip $ xs

-- |Lifts a function 'f: [a] -> [b]'' to a ParArrow.
--  'f' has to preserve the order of the elements in its input-list,
--  i.e. xs !! i must be mapped 1:1 to f xs !! i.
-- 
-- History-erasing version of @liftPar'@.
liftParErase' :: (Monad m)
         => ([a] -> [b])
         -> ParArrow m a a b
liftParErase' f = liftParErase $ uncurry zip . (id &&& f)

-- |Concatenates two ParArrows.
--  If the first fails globally, the concatenated ParArrow
--  fails globally too. If not, the first ParArrow is
--  applied, all local failures are removed, and after that,
--  the second ParArrow is applied.
(=|>) :: (MonadFilter m)
      => ParArrow m a b c
      -> ParArrow m a c d
      -> ParArrow m a b d
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
