-- |Assorted REPL (read-eval-print-loop) functionality for console UIs.
module Console.REPL (
   repl',
   repl,

   askFor,
   askForBy',
   askForBy,

   putPrompt) where

import Control.Monad
import System.IO
import Text.Read

-- |Executes a REP loop until a given condition is met.
--  Variant of @repl'@ which performs the same input and output action
--  over and over again.
repl' :: Monad m => (a -> Bool) -> m a -> (a -> m b) -> m ()
repl' check f g = repl check (repeat f) (repeat g)

-- |Executes a REP loop until a given condition is met.
repl :: Monad m
     => (a -> Bool) -- |The check which, when true, terminates the loop.
     -> [m a] -- |The list of inputs.
     -> [a -> m b] -- |The list of functions operating on the input.
     -> m ()
repl check i o = sequenceWhile check $ zip i o
  where sequenceWhile _ [] = return ()
        sequenceWhile f ((inp,out):xs) =
           do inp' <- inp
              unless (f inp') $ do out inp'
                                   sequenceWhile f xs

-- |Asks for a value of a given type.
--  The prompt is printed and the user is asked to enter
--  a line. If the input cannot be parsed, the error text
--  is printed and the process repeats.
askFor :: Read a => String -> String -> IO a
askFor prompt err = askForBy' prompt err (const (return True))

-- |Variant of @askFor@, with an additional predicate which the
--  input must satisify.
askForBy' :: Read a => String -> String -> (a -> IO Bool) -> IO a
askForBy' prompt err check = do
   x <- putPrompt prompt
   case readMaybe x of
      Just x' -> do isValid <- check x'
                    if isValid then return x'
                    else putStrLn err >> askForBy' prompt err check
      Nothing -> putStrLn err >> askForBy' prompt err check

-- |Variant of @askFor@, with an additional predicate which the
--  input must satisify.
askForBy :: String -> String -> (String -> IO Bool) -> IO String
askForBy prompt err check = do
   x <- putPrompt prompt
   isValid <- check x
   if isValid then return x
   else putStrLn err >> askForBy prompt err check

-- |Puts a prompt onto the console without starting a new line.
putPrompt :: String -> IO String
putPrompt prompt = putStr prompt >> hFlush stdout >> getLine
