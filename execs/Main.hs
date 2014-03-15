
module Main where


import qualified Data.Map           as M
import           System.Environment

import           MinCaml.KNormal
import           MinCaml.Parser
import           MinCaml.Typing


main :: IO ()
main = do
    args <- getArgs
    tm <- parseFile' (args !! 0)
    case tm of
      Left err -> print err
      Right (tm', tyvar) ->
        case runUnify (infer init_env tm' >>= prune) (TypingState M.empty tyvar) of
          Left err -> print err
          Right (ty, st) -> do
            print ty
            case evalUnify (eliminateTyVars tm') st of
              Left err -> print err
              Right tm'' -> print $ knormal init_env tm''
