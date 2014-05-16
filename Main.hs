-- |A console-based program which renames files in a directory.
module Main (main) where

import System.Directory
import Control.Monad
import Control.Arrow
import Control.Arrow.ParArrow
import Control.Arrow.Utils
import Control.Monad.State.Lazy
import Console.REPL
import Data.Maybe
import qualified Data.List as LS
import Data.Renaming
import Data.Ord
import System.FilePath (combine)
import qualified Algorithms.NaturalSort as NS (compare)

data Asc = Asc | Desc deriving (Show, Eq, Ord, Read, Enum)

type AppState = StateT String IO ()

main :: IO ()
main = getCurrentDirectory >>= evalStateT main'
  where
    main' :: AppState
    main' = repl' isExitCommand (liftIO $ putPrompt "> ") runCommand

    runCommand c = fromMaybe (liftIO $ putStrLn "Command not found. Type :help for a list of commands.")
                             (lookup c commandLib)

    isExitCommand = (`elem` exitCommands) . trim
      where exitCommands = [":e", ":exit", ":quit", ":q"]

    trim = unwords . words


commandLib :: [(String, AppState)]
commandLib = [
   (":help", do liftIO $ putStrLn "Renamer"
                liftIO $ putStrLn "Commands: :cd, :pipe, :exit"
                dir <- get
                liftIO $ putStrLn $ "Current directory: " ++ dir
   ),
   -- Change current directory
   (":cd", do dir <- liftIO $ askForBy "Enter new directory: "
                                       "Directory not found!"
                                       doesDirectoryExist
              put dir
   ),
   -- Run a pipe
   (":pipe", do dir <- get
                liftIO (
                  do files' <- getDirectoryContents dir
                     let files = LS.sortBy NS.compare files'
                     pipeName <- askForBy "Enter pipe name: "
                                          "Pipe not found!"
                                          (return
                                           . isJust
                                           . flip lookup pipeLib)
                     let pipe = fromJust $ lookup pipeName pipeLib
                     renamings <- pipe dir files
                     putStrLn "Proposed renamings: "
                     mapM_ (print' (maximum $ map length files)) renamings
                     proceed <- askFor "Proceed (True/False)? " "True/False required!"
                     let addDir = combine dir *** liftM (combine dir)
                     if proceed then do renameFiles (map addDir renamings)
                                        putStrLn "Finished."
                                else putStrLn "Didn't do anything.")
      )
   ]


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
             sorter = mapPar (addInfoM modtime >>> flattenET)
                      =|> sortBy (cmp $ comparing snd)
                      =|> mapPar (liftP fst)
                      =|> liftParErase' id
                      =|> mapPar (splitExt >< liftP snd)
                      =|> addInfoG (map show ([1..]::[Integer]))
                      =|> mapPar (liftP $ \(e,n) -> (n++e))
         applyRenamings sorter files),
   ("enumerate", \_ files ->
      do start <- askFor "Enter start value (Integer): " "Start value required!" :: IO Integer
         let f = mapPar (splitExt >< liftP snd)
                 =|> addInfoG [start..]
                 =|> mapPar (switch <> (liftP show, liftP id) >< addExt)
         applyRenamings f files)
   ]

-- |Prints a pair of old/new filenames, with a given length
--  to which to pad.
print' :: Int -> (String, Either String String) -> IO ()
print' n (old, new) = putStrLn $ padRight n old ++ "-->" ++ show new

-- |Pads a string with characters to its right until the given length
--  is reached.
padRight :: Int -> String -> String
padRight n xs = xs ++ replicate (n - length xs) ' '


-- For testing

-- |Runs a ParArrow and prints its results.
--runP :: (Show a, Show c) => ParArrow (EitherT String IO) a b c -> [(a,b)] -> IO ()
--runP p xs = do (Right xs') <- runEitherT $ p xs
--               mapM_ (f >=> print) xs'
--  where f (a,b) = runEitherT b >>= \(Right b') -> return (a,b')

-- |Fixes a pipe's monad to 'EitherT String IO' (for testing)
--eitherP :: ParArrow (EitherT String IO) a b c -> ParArrow (EitherT String IO) a b c
--eitherP = id
