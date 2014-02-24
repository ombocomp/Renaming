-- |A console-based program which
module Main where

import System.Directory
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Renaming
import System.IO
import System.FilePath (combine, splitExtensions)
import Text.Read
import qualified Algorithms.NaturalSort as NS (compare)

-- |Strips the extension from a file.
--  History effect: @xs --> extension:filename:xs@
stripExtension :: Renaming String
stripExtension = Renaming stripExt
   where stripExt (p,xs) = (name, ext:p:xs)
            where (name, ext) = splitExtensions p

-- |Adds the extension back to a file, provided it's
--  in the history.
--  History effect: @[] --> [fileName]@ (if the history is empty)
--  or @xs --> filename:xs@.
addExtension :: Renaming String
addExtension = Renaming f
   where f (p, []) = (p, [p])
         f (p, x:xs) = (p++x, p:x:xs)

-- |Returns True iff the filename can be parsed as an Integer.
isInteger :: (FilePath, a) -> Bool
isInteger (p,_) = isJust (readMaybe p::Maybe Integer)

-- |Interprets the filename as an integer and applies a function
--  to it.
--  History effect: none.
mapInt :: (Integer -> Integer) -> Renaming a
mapInt f = guardR isInteger mapInt'
   where mapInt' = simpleR (show . fromJust . liftM f . readMaybe)

-- |Applies a renaming to a filename, returning the old and new names.
applyRenaming :: Renaming a -> FilePath -> (FilePath, FilePath)
applyRenaming r p = (p, finishR r (p, mempty))

-- |Performs a series of file renamings. The first component of each
--  tuple is the current filename, the second is the new one.
renameFiles :: [(FilePath, FilePath)] -> IO ()
renameFiles files = do
   let temp = flip (++) ".move" 
   files' <- filterM (\(x,y) -> liftM ((x/=y) &&) (doesFileExist x)) files
   mapM_ (\(x,_) -> renameFile x (temp x)) files'
   mapM_ (\(x,y) -> renameFile (temp x) y) files'

main :: IO ()
main =
   let print' n (old, new) = putStrLn $ padRight n old ++ " --> " ++ new  in
   do dir <- askForBy "Enter directory name: " "Directory not found!" doesDirectoryExist
      files'' <- getDirectoryContents dir
      files' <- filterM (doesFileExist . combine dir) files''
      let files = sortBy (NS.compare) files'
      amount <- askFor "Enter amount: " "Integer required!"
      let f = stripExtension <> mapInt (+amount) <> addExtension
          renamings = map (applyRenaming f) files
          addDir = combine dir *** combine dir
          maxNameLength = maximum $ map length files
      putStrLn "Proposed renamings: "
      mapM_ (print' maxNameLength) $ renamings
      proceed <- askFor "Proceed (True/False)? " "True/False required!"
      if proceed then renameFiles (map addDir renamings) >> putStrLn "Finished."
      else putStrLn "Exiting without action."



askFor :: Read a => String -> String -> IO a
askFor prompt err = askForBy' prompt err (const (return True))

askForBy' :: Read a => String -> String -> (a -> IO Bool) -> IO a
askForBy' prompt err check = do
   putStr prompt
   hFlush stdout
   x <- getLine
   case readMaybe x of
      Just x' -> do isValid <- check x'
                    if isValid then return x'
                    else putStrLn err >> askForBy' prompt err check
      Nothing -> putStrLn err >> askForBy' prompt err check

askForBy :: String -> String -> (String -> IO Bool) -> IO String
askForBy prompt err check = do
   putStr prompt
   hFlush stdout
   x <- getLine
   isValid <- check x
   if isValid then return x
   else putStrLn err >> askForBy prompt err check

padRight :: Int -> String -> String
padRight n xs = xs ++ replicate (n - length xs) ' '