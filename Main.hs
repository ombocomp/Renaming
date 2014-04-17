-- |A console-based program which renames files in a directory.
module Main (main) where

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

import Control.Monad.Trans.Either

data Asc = Asc | Desc deriving (Show, Eq, Ord, Read, Enum)

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
      do asc <- askFor "Enter direction (Asc/Desc):" "Asc/Desc required!"
         let cmp = if asc == Asc then id else flip
             modtime x = getModificationTime (combine dir x)
             sorter = mapPar (addInfoM modtime >=> flattenET)
                      =|> sortBy (cmp $ comparing snd)
                      =|> mapPar (liftP fst)
                      =|> liftParErase' id
                      =|> mapPar (splitExt >< liftP snd)
                      =|> addInfoG (map show ([1..]::[Integer]))
                      =|> mapPar (liftP $ \(e,n) -> (n++e))
         applyRenamings sorter files),
   ("enumerate", \dir files ->
      do start <- askFor "Enter start value (Integer): " "Start value required!" :: IO Integer
         let f = mapPar (splitExt >< liftP snd)
                 =|> addInfoG [start..]
                 =|> mapPar (switch idP <> (liftP show, liftP id) >< addExt)
         applyRenamings f files)
   ]


-- For testing

-- |Runs a ParPipe and prints its results.
runP :: (Show a, Show c) => ParPipe (EitherT String IO) a b c -> [(a,b)] -> IO ()
runP p xs = do (Right xs') <- runEitherT $ p xs
               mapM_ (f >=> print) xs'
  where f (a,b) = runEitherT b >>= \(Right b') -> return (a,b')

-- |Fixes a pipe's monad to 'EitherT String IO' (for testing)
eitherP :: ParPipe (EitherT String IO) a b c -> ParPipe (EitherT String IO) a b c
eitherP = id