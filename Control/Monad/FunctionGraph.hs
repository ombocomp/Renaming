{-|Functions to create directed graphs of functions through which
   data can be sent, in an ordered manner; quite similar to Kleisli arrows.

   Encourages, in essence, point-free programming and transformation-centric
   view of programs.
-}
module Control.Monad.FunctionGraph (

  -- * Lifting & composing functions
  liftP,
  (>=>),

  -- * Splitting inputs
  (<>),
  (<>>),
  (<>>>),
  (<>>>>),
  (<>*),

  -- ** Convenience functions
  copy,
  copy3,
  copy4,
  copy5,

  switch,

  -- * Merging outputs
  (><),

  -- * Executing functions in parallel
  liftPar,
  (=|>),

  -- * Guards
  guard,
  (??),
  failIf,
  checkThat,

  -- * Type synonims for common pieces
  Pipe,
  ParPipe,

  -- ** Splitters
  Splitter,
  ThreeSplitter,
  FourSplitter,
  FiveSplitter,
  ManySplitter,

  -- ** Mergers
  Merger,
  ThreeMerger,
  FourMerger,
  FiveMerger,
  ManyMerger,

  ) where

import Control.Monad(MonadPlus(..))
import Control.Monad.Error ((>=>))
import Control.Arrow
import Control.Applicative

-- |A basic (monadic) function.
type Pipe m a b = a -> m b

-- |A function with two outputs.
type Splitter m a b c = a -> m (b,c)
-- |A function with three outputs.
type ThreeSplitter m a b c d = a -> m (b,c,d)
-- |A function with four outputs.
type FourSplitter m a b c d e = a -> m (b,c,d,e)
-- |A function with five outputs.
type FiveSplitter m a b c d e f = a -> m (b,c,d,e,f)
-- |A function with a list of outputs.
type ManySplitter m a b = a -> m [b]

-- |A function with two inputs.
type Merger m a b c = (a,b) -> m c
-- |A function with three inputs.
type ThreeMerger m a b c d = (a,b,c) -> m d
-- |A function with four inputs.
type FourMerger m a b c d e = (a,b,c,d) -> m e
-- |A function with five inputs.
type FiveMerger m a b c d e f = (a,b,c,d,e) -> m f
-- |A function with a list of inputs.
type ManyMerger m a b = [a] -> m b

-- |A parallel execution which has both local and
--  global state (the computation may fail in certain
--  places, or as a whole).
type ParPipe m a b = [a] -> m [m b]

-- |Wraps the return value of a function into a monad.
liftP :: Monad m => (a -> b) -> Pipe m a b
liftP = (return .)

-- |Splits an input into two parts and gives each to a handler.
(<>) :: Monad m
     => Splitter m a b c
     -> (Pipe m b d, Pipe m c e)
     -> Splitter m a d e
(<>) f (g,h) x = do (y,z) <- f x
                    y' <- g y
                    z' <- h z
                    return (y',z')

-- |Splits an input into three parts and gives each to a handler.
(<>>) :: Monad m
      => ThreeSplitter m a b c d
      -> (Pipe m b e, Pipe m c f, Pipe m d g)
      -> ThreeSplitter m a e f g
(<>>) f (g,h,i) x = do (y,z,u) <- f x
                       y' <- g y
                       z' <- h z
                       u' <- i u
                       return (y',z',u')

-- |Splits an input into four parts and gives each to a handler.
(<>>>) :: Monad m
       => FourSplitter m a b c d e
       -> (Pipe m b f, Pipe m c g, Pipe m d h, Pipe m e i)
       -> FourSplitter m a f g h i
(<>>>) f (g,h,i,j) x = do (y,z,u,v) <- f x
                          y' <- g y
                          z' <- h z
                          u' <- i u
                          v' <- j v
                          return (y',z',u',v')

-- |Splits an input into five parts and gives each to a handler.
(<>>>>) :: Monad m
       => FiveSplitter m a b c d e f
       -> (Pipe m b g, Pipe m c h, Pipe m d i, Pipe m e j, Pipe m f k)
       -> FiveSplitter m a g h i j k
(<>>>>) f (g,h,i,j,k) x = do (y,z,u,v,w) <- f x
                             y' <- g y
                             z' <- h z
                             u' <- i u
                             v' <- j v
                             w' <- k w
                             return (y',z',u',v',w')

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
liftPar :: Monad m
        => Pipe m a b
        -> ParPipe m a b
liftPar p = return . map p

-- |Concatenates two parPipes.
--  If the first fails globally, the concatenated parPipe
--  fails globally too. If not, the first parPipe is
--  applied, all local failures are removed, and the second
--  parPipe is applied.
(=|>) :: (MonadPlus m, Eq (m b))
      => ParPipe m a b
      -> ParPipe m b c
      -> ParPipe m a c
(=|>) p q x = q =<< sequence . filter (mzero==) =<< p x

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

-- |Inverse of 'failIf': only proceeds if a certain condition is
--  met and induces a global failure otherwise.
checkThat :: MonadPlus m => (a -> Bool) -> Pipe m a a -> Pipe m a a
checkThat f = failIf (not . f)

copy :: Monad m => Splitter m a a a
copy =  liftP (id &&& id)

copy3 :: Monad m => ThreeSplitter m a a a a
copy3 =  liftP (\x -> (x,x,x))

copy4 :: Monad m => FourSplitter m a a a a a
copy4 =  liftP (\x -> (x,x,x,x))

copy5 :: Monad m => FiveSplitter m a a a a a a
copy5 =  liftP (\x -> (x,x,x,x,x))

-- |Switches the outputs of a splitter.
switch :: Monad m => Splitter m a b c -> Splitter m a c b
switch = (flip (>=>)) $ liftP (\(x,y) -> (y,x))

