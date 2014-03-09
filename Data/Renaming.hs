-- |Functions for en masse renaming of files in a folder.
module Data.Renaming (
   -- * IO Actions
   applyRenamings,
   renameFiles,

   -- * Example renaming functions
   mapInt,
   addInfo,
   sortBy,

   -- ** Extensions
   splitExt,
   addExt,
   mapExt,

   -- ** Data types and numbers
   isInteger,
   isNumber,
   isDateTime

   ) where

import qualified Data.List as LS (sortBy)
import Control.Applicative
import Control.Monad.FunctionGraph
import System.Directory
import Control.Monad
import Control.Arrow
import Data.Maybe
import System.IO
import System.FilePath (combine, splitExtensions)
import Text.Read
import qualified Algorithms.NaturalSort as NS (compare)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromRight :: Either a b -> b
fromRight (Right x) = x

-- |Splits a file into filename and extension (the extension is
--  taken to start at the first dot (.) ).
splitExt :: Splitter (Either String) FilePath FilePath FilePath
splitExt = liftP splitExtensions

-- |Merges a file and its extension together.
addExt :: Merger (Either String) FilePath FilePath FilePath
addExt = liftP (uncurry (++))

-- |Returns True iff the filename can be parsed as an Integer.
isInteger :: String -> Bool
isInteger = isJust . (readMaybe :: String -> Maybe Integer)

-- |Interprets the filename as an integer and applies a function to it.
mapInt :: (Integer -> Integer) -> Pipe (Either String) String String
mapInt f = splitExt <> (isInteger ?? f', liftP id) >< addExt
  where f' = liftP $ show . f . read

-- |Applies a function to a file's extension
mapExt :: Pipe (Either String) String String
       -> Pipe (Either String) String String 
mapExt p = splitExt <> (liftP id, p) >< addExt

-- |Adds some auxiliary information to a filename (such as date modified,
--  size, etc.).
addInfo :: (FilePath -> IO a)
        -> Splitter (Either String) FilePath FilePath (IO a)
addInfo f = liftP $ (id &&& f)

-- |Sorts a list of filenames.
sortBy :: (a -> a -> Ordering)
       -> ParPipe (Either String) a a
sortBy f = liftPar (LS.sortBy f)

-- |Applies a parPipe to a set of filenames,
--  returning tuples of current and new filenames where
--  
--  1. the renaming was sucessful and
--  2. the new filename doesn't equal the old one.
applyRenamings :: ParPipe (Either String) FilePath FilePath
              -> [FilePath]
              -> [(FilePath, FilePath)]
applyRenamings p xs =
  case p xs of Left _ -> []
               Right xs' -> filter (uncurry (/=))
                            $ map (second fromRight)
                            $ filter (isRight . snd)
                            $ xs `zip` xs'

-- |Performs a series of file renamings. The first component of each
--  tuple is the current filename, the second is the new one.
--
--  Transaction guarantees (ACID) are NOT given. Each renaming
--  is performed in two steps: first, a file @X@ is renamed to
--  @X.move@ and then @X.move" is renamed to the new name.

--  This is a primitive, but mostly sufficient,
--  conflict-avoidance scheme intended
--  to work around the limitations of file systems.
renameFiles :: [(FilePath, FilePath)] -> IO ()
renameFiles files =
  do let temp = flip (++) ".move"
     files' <- filterM (doesFileExist . fst) files
     mapM_ (\(x,_) -> renameFile x (temp x)) files'
     mapM_ (\(x,y) -> renameFile (temp x) y) files'