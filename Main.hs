-- |A console-based program which renames files in a directory.
module Main where

import System.Directory
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.List
import Control.Monad.FunctionGraph
import System.IO
import System.FilePath (combine, splitExtensions)
import Text.Read
import qualified Algorithms.NaturalSort as NS (compare)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromRight :: Either a b -> b
fromRight (Right x) = x

splitExt :: Splitter (Either String) FilePath FilePath FilePath
splitExt = liftP splitExtensions
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

-- |Applies a parPipe to a set of filenames.
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
renameFiles :: [(FilePath, FilePath)] -> IO ()
renameFiles files =
  do let temp = flip (++) ".move"
     files' <- filterM (doesFileExist . fst) files
     mapM_ (\(x,_) -> renameFile x (temp x)) files'
     mapM_ (\(x,y) -> renameFile (temp x) y) files'

main :: IO ()
main =
   let print' n (old, new) = putStrLn $ padRight n old ++ " --> " ++ new  in
   do dir <- askForBy "Enter directory name: " "Directory not found!" doesDirectoryExist
      files' <- getDirectoryContents dir
      let files = sortBy NS.compare files'
      amount <- askFor "Enter amount: " "Integer required!"
      let f = liftPar $ mapInt (+amount)
          renamings = applyRenamings f files
          addDir = combine dir *** combine dir
          maxNameLength = maximum $ map length files
      putStrLn "Proposed renamings: "
      mapM_ (print' maxNameLength) renamings
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