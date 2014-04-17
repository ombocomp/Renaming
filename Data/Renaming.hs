{-# LANGUAGE TupleSections #-}

-- |Functions for en masse renaming of files in a folder.
module Data.Renaming (
   -- * IO Actions
   applyRenamings,
   renameFiles,
   mkEitherT,
   flattenET,

   -- * Example renaming functions
   mapInt,
   addInfoM,
   addInfoG,
   sortBy,
   filterSpecialFiles,

   -- ** Extensions
   mapName,
   splitExt,
   addExt,
   mapExt,

   -- ** Data types and numbers
   isInteger,
   isDouble

   ) where

import qualified Data.List as LS (sortBy)
--import Control.Applicative
import Control.Monad.FunctionGraph
import System.Directory
import Control.Monad.Trans.Either
import Control.Monad
import Control.Arrow
import Data.Maybe
import System.FilePath (splitExtensions)
import Text.Read
--import qualified Algorithms.NaturalSort as NS (compare)
                              

-- |Flattens a value '(b, m c)' into 'EitherT String m (b, c)', bringing
--  the monadic component out of the tuple. Useful when previous
--  pipes added monadic components like file sizes or timestamps.
flattenET :: (Monad m)
          => Pipe (EitherT String m) (b, m c) (b, c)
flattenET (x,y) = EitherT $ liftM (Right . (x,)) y

-- |Returns True iff the Either-object has a right value.
isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

-- |Returns m True iff the EitherT-object has a right value.
isRightT :: Monad m
        => EitherT a m b
        -> m Bool
isRightT x = do x' <- runEitherT x
                case x' of (Left _) -> return False
                           _        -> return True

-- |Extracts the right value from an Either.
fromRight :: Either a b -> b
fromRight (Right x) = x

-- |Extracts the right value from an EitherT.
fromRightT :: Monad m
          => EitherT a m b
          -> m b
fromRightT x = runEitherT x >>= (\(Right x') -> return x')

-- |Turns a monadic function (e.g. a -> IO b) into one that returns EitherT.
mkEitherT :: Monad m
          => (a -> m b)
          -> a
          -> EitherT String m b
mkEitherT f x = EitherT $ liftM Right (f x)

-- |Splits a file into filename and extension (the extension is
--  taken to start at the first dot (.) ).
splitExt :: Monad m
         => Splitter (EitherT String m) FilePath FilePath FilePath
splitExt = liftP splitExtensions

-- |Merges a file and its extension together.
addExt :: Monad m => Merger (EitherT String m) FilePath FilePath FilePath
addExt = liftP (uncurry (++))

-- |Returns True iff the filename can be parsed as an Integer.
isInteger :: String -> Bool
isInteger = isJust . (readMaybe :: String -> Maybe Integer)

-- |Returns True iff the filename can be parsed as a Double.
isDouble :: String -> Bool
isDouble = isJust . (readMaybe :: String -> Maybe Double)

-- |Interprets the filename as an integer and applies a function to it.
mapInt :: Monad m
       => (Integer -> Integer)
       -> Pipe (EitherT String m) String String
mapInt f = splitExt <> (isInteger ?? f', liftP id) >< addExt
  where f' = liftP $ show . f . read

-- |Applies a function to a file's name (leaves the extension intact).
mapName :: Monad m
        => (String -> String)
        -> Pipe (EitherT String m) String String
mapName f = splitExt <> (liftP f, liftP id) >< addExt

-- |Applies a function to a file's extension
mapExt :: Monad m
       => Pipe (EitherT String m) String String
       -> Pipe (EitherT String m) String String 
mapExt p = splitExt <> (liftP id, p) >< addExt

-- |Adds some monadic auxiliary information to a filename
-- (such as date modified, size, etc.).
addInfoM :: Monad m
         => (FilePath -> m a)
         -> Splitter (EitherT String m) FilePath FilePath (m a)
addInfoM f = liftP (id &&& f)

-- |Adds some global information to a list of filenames
--  (such as a numbering).
addInfoG :: (Monad m, Eq b)
             => [c]
             -> ParPipe (EitherT String m) a b (b, c)
addInfoG xs = liftPar' (`zip` xs)

-- |Sorts a list of filenames.
sortBy :: (Monad m, Eq b)
       => (b -> b -> Ordering)
       -> ParPipe (EitherT String m) a b b
sortBy f = liftPar' (LS.sortBy f)

-- |Causes renaming failures in case of special filenames (. and ..).
filterSpecialFiles :: (Monad m)
                   => ParPipe (EitherT String m) a String String
filterSpecialFiles = mapPar $ failIfEither (`elem` special) "special character!" (liftP id)
   where special = [".", ".."]

-- |Applies a parPipe to a set of filenames,
--  returning tuples of current and new filenames where
--  the renaming was sucessful or appropriate error messages
--  in the case of failures.
applyRenamings :: Monad m
              => ParPipe (EitherT String m) FilePath FilePath b
              -> [FilePath]
              -> m [(FilePath, Either String b)]
applyRenamings p xs =
  do res <- runEitherT $ filterSpecialFiles =|> p $ (xs `zip` xs)
     case res of Left _ -> return []
                 Right ys -> sequence' ys
   where
      sequence' [] = return []
      sequence' ((a,b):xs) = do b' <- runEitherT b
                                xs' <- sequence' xs
                                return ((a,b'):xs')

-- |Performs a series of file renamings. The first component of each
--  tuple is the current filename, the second is the new one.
--
--  Transaction guarantees (ACID) are NOT given. Each renaming
--  is performed in two steps: first, a file @X@ is renamed to
--  @X.move@ and then @X.move" is renamed to the new name.

--  This is a primitive, but mostly sufficient,
--  conflict-avoidance scheme intended
--  to work around the limitations of file systems.
renameFiles :: [(FilePath, Either String FilePath)] -> IO ()
renameFiles files =
  do let temp = flip (++) ".move"
         files' = map (second fromRight) $ filter (isRight . snd) files
     files'' <- filterM (doesFileExist . fst) files'
     mapM_ (\(x,_) -> renameFile x (temp x)) files''
     mapM_ (\(x,y) -> renameFile (temp x) y) files''
