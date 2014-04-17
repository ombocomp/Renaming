-- |A console-based program which renames files in a directory.
module Main where

import System.Directory
import Control.Monad
import Control.Arrow
import Data.Maybe
import qualified Data.List as LS
import Control.Monad.FunctionGraph
import Data.Renaming
import Data.Ord
import System.IO
import System.FilePath (combine)
import Text.Read
import qualified Algorithms.NaturalSort as NS (compare)

main :: IO ()
main =
   do dir <- askForBy "Enter directory name: " "Directory not found!" doesDirectoryExist
      files' <- getDirectoryContents dir
      let files = LS.sortBy NS.compare files'
      pipeName <- askForBy "Enter pipe name: " "Pipe not found!" (return .
                                                                  isJust .
                                                                  flip lookup pipeLib)
      let pipe = fromJust $ lookup pipeName pipeLib
      renamings <- pipe dir files
      putStrLn "Proposed renamings: "
      mapM_ (print' (maximum $ map length files)) renamings
      proceed <- askFor "Proceed (True/False)? " "True/False required!"
      let addDir = combine dir *** liftM (combine dir)
      if proceed then renameFiles (map addDir renamings) >> putStrLn "Finished."
      else putStrLn "Exiting without action."

print' :: Int -> (String, Either String String) -> IO ()
print' n (old, new) = putStrLn $ padRight n old ++ "-->" ++ show new

askFor :: Read a => String -> String -> IO a
askFor prompt err = askForBy' prompt err (const (return True))

askForBy' :: Read a => String -> String -> (a -> IO Bool) -> IO a
askForBy' prompt err check = do
   x <- putPrompt prompt
   case readMaybe x of
      Just x' -> do isValid <- check x'
                    if isValid then return x'
                    else putStrLn err >> askForBy' prompt err check
      Nothing -> putStrLn err >> askForBy' prompt err check

askForBy :: String -> String -> (String -> IO Bool) -> IO String
askForBy prompt err check = do
   x <- putPrompt prompt
   isValid <- check x
   if isValid then return x
   else putStrLn err >> askForBy prompt err check

putPrompt :: String -> IO String
putPrompt prompt = putStr prompt >> hFlush stdout >> getLine

padRight :: Int -> String -> String
padRight n xs = xs ++ replicate (n - length xs) ' '

pipeLib :: [(String, String -> [String] -> IO [(String, Either String String)])]
pipeLib = [
   ("addN", \_ files ->
      do amount <- askFor "Enter amount: " "Integer required!"
         let f = mapPar $ mapInt (+amount)
         applyRenamings f files),
   ("renameCD", const $ applyRenamings (mapPar $ mapName (take 2))),
   ("sortByDate", \dir files ->
      do let modtime x = getModificationTime (combine dir x)
         let sorter = mapPar (addInfoM modtime >=> flattenET)
                      =|> sortBy (comparing snd)
                      =|> mapPar (liftP fst)
                      =|> mapPar (splitExt >< liftP snd)
                      =|> addInfoG (map show ([1..]::[Integer]))
                      =|> mapPar (liftP $ \(e,n) -> (n++e))
         applyRenamings sorter files)
   ]
