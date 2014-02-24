-- |Contains the 'Renaming' monoid which encodes the
--  process of renaming a file in multiple stages.
module Data.Renaming (
   Renaming(..),
   liftR,
   simpleR,
   finishR,
   guardR
   ) where

import System.Directory
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.List
import Data.Monoid
import System.IO
import System.FilePath (combine, splitExtensions)
import Text.Read

-- |A file renaming process which maps a filename to a filename,
--  while also keeping a history of performed changes.
newtype Renaming a = Renaming{runRenaming::(FilePath, [a]) -> (FilePath,[a])}

instance Monoid (Renaming a) where
   -- |The identity 'Renaming' which doesn't leaves both
   --  the filename and the history unchanged.
   mempty = Renaming (first id)
   -- |Performs two 'Renaming's in sequence.
   mappend r1 r2 = Renaming $ runRenaming r2 . runRenaming r1

-- |Takes a function which maps a filename to a filename
--  and lifts it to a 'Renaming'.
liftR :: (FilePath -> (FilePath,[a])) -> Renaming a
liftR f = Renaming (\(p,_) -> f p)

-- |Creates a 'Renaming' which doesn't have a history.
simpleR :: (FilePath -> FilePath) -> Renaming a
simpleR f = Renaming (first f)

-- |Runs a 'Renaming', delivering the result and discarding
--  the history.
finishR :: Renaming a -> (FilePath, [a]) -> FilePath
finishR r = fst . runRenaming r

-- |Places a guard in front of a renaming:
--  if the input fails the guard function,
--  the identity 'Renaming' is returned instead.
guardR :: ((FilePath, [a]) -> Bool) -> Renaming a -> Renaming a
guardR f r = Renaming (\p -> if f p then runRenaming r p else runRenaming mempty p)
