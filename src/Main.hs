
module Main where


import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Map               as M
import           Parser
import           System.Environment

import           Typing


main :: IO ()
main = do
    args <- getArgs
    tm <- parseFile' (args !! 0)
    case tm of
      Left err -> print err
      Right (tm', tyvar) ->
        print $ evalUnify (infer M.empty tm' >>= prune) (TypingState M.empty tyvar)
