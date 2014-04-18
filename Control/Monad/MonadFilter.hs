module Control.Monad.MonadFilter where

import Control.Exception (catch, SomeException)
import Control.Monad hiding (mfilter)
import Control.Monad.Error hiding (mfilter)
import Control.Monad.Trans.Either
import Data.Maybe (catMaybes)
import Data.Either (rights)
import Data.Monoid

-- |Monadic filters which can filter
--  invalid elements from a list of monadic values, based on MonadPlus.
--  Minimal complete definition: 'mfilter'
--  MonadFilter laws:

--  1. mfilter xs = [x | x <- xs, x /= mzero]
--  2. ismzero x iff mfold [x] is null 
class MonadPlus m => MonadFilter m where
   -- |Iterates over a list of monadic values, discarding
   --  those which are mzero.
   mfilter :: [m a] -> m [a]
   -- |Returns whether a value equals mzero. The default implementation
   --  uses @mfold@.
   ismzero :: m a -> m Bool
   ismzero = liftM null . mfilter . (:[])
   -- |Alternative to @ismzero@ which returns 'Nothing' if a value is
   --  mzero and 'Just a' otherwise.
   ismzero' :: m a -> m (Maybe a)
   ismzero' = liftM head' . mfilter . (:[])
      where head' [] = Nothing
            head' (x:_) = Just x

instance MonadFilter Maybe where
   mfilter = return . catMaybes

instance MonadFilter [] where
   mfilter = filter (not . null)

instance Error a => MonadFilter (Either a) where
   mfilter = return . rights

instance MonadFilter IO where
   mfilter [] = return []
   mfilter (x:xs) = do {x' <- x; xs' <- mfilter xs; return (x':xs')} `catchAny`
                    const (mfilter xs)
      where catchAny :: IO a -> (SomeException -> IO a) -> IO a
            catchAny = catch

instance (Monoid a, Monad m) => MonadFilter (EitherT a m) where
   mfilter [] = return []
   mfilter (x:xs) = EitherT $ do x' <- runEitherT x
                                 (Right xs') <- runEitherT $ mfilter xs
                                 case x' of Left _ -> return (Right xs')
                                            Right x'' -> return $ Right (x'':xs')
