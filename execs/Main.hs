
module Main where


import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Map               as M
import           System.Environment

import           MinCaml.KNormal
import           MinCaml.Parser
import           MinCaml.Types
import           MinCaml.Typing


main :: IO ()
main = do
    args <- getArgs
    tm <- parseFile' (args !! 0)
    case tm of
      Left err -> print err
      Right (tm', tyvar) ->
        let init_env = M.singleton "print_int" (TyFun [TyInt] TyUnit) in
        case evalUnify (infer init_env tm' >>= prune) (TypingState M.empty tyvar) of
          Left err -> print err
          Right ty -> do
            print $ knormal init_env tm'
