
module Main where


import qualified Data.Map           as M
import           System.Environment

import           MinCaml.AlphaConv
import           MinCaml.KNormal
import           MinCaml.Parser
import           MinCaml.Typing
import           MinCaml.ClosureConv as CC


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
              Right tm'' -> do
                let (kn, _) = knormal init_env tm''
                print kn

                putStrLn "alpha conversion -----------------------"
                let kn' = alphaConv kn
                print kn'

                putStrLn "flatten --------------------------------"
                let kn'' = flatten kn'
                print kn''

                putStrLn "closure conversion ---------------------"
                let (c, defs) = CC.closureConv kn''
                print defs
                print c
