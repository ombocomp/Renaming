-- |A console-based program which renames files in a directory.
module Main where

import System.Directory
import Control.Monad
import Control.Arrow
import Data.Maybe
import qualified Data.List as LS
import Control.Monad.FunctionGraph
import Data.Renaming
import System.IO
import System.FilePath (combine, splitExtensions)
import Text.Read
import qualified Algorithms.NaturalSort as NS (compare)



main :: IO ()
main =
   let print' n (old, new) = putStrLn $ padRight n old ++ " --> " ++ new  in
   do dir <- askForBy "Enter directory name: " "Directory not found!" doesDirectoryExist
      files' <- getDirectoryContents dir
      let files = LS.sortBy NS.compare files'
      pipeName <- askForBy "Enter pipe name: " "Pipe not found!" (return .
                                                                  isJust .
                                                                  flip lookup pipeLib)
      let pipe = fromJust $ lookup pipeName pipeLib
      renamings <- pipe files
      putStrLn "Proposed renamings: "
      mapM_ (print' (maximum $ map length files)) renamings
      proceed <- askFor "Proceed (True/False)? " "True/False required!"
      let addDir = combine dir *** combine dir
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

pipeLib :: [(String, [String] -> IO [(String, String)])]
pipeLib = [
   ("addN", \files ->
      do amount <- askFor "Enter amount: " "Integer required!"
         let f = mapPar $ mapInt (+amount)
         return $ applyRenamings f files),
   ("renameCD", \files ->
      return $ applyRenamings (mapPar $ mapName (take 2)) files)
   ]