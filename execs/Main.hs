module Main where

import qualified Data.Map                  as M
import           System.Environment
import           Text.PrettyPrint.HughesPJ hiding (render)

import qualified MinCaml.AlphaConv         as AC
import qualified MinCaml.ClosureConv       as CC
import qualified MinCaml.Codegen.C         as Codegen
import qualified MinCaml.KNormal           as KN
import           MinCaml.Parser
import           MinCaml.Typing

render :: Doc -> IO ()
render = putStrLn . renderStyle (Style PageMode 80 0.9)

main :: IO ()
main = do
    args <- getArgs
    tm <- parseFile' (args !! 0)
    case tm of
      Left err -> print err
      Right (tm', tyvar) ->
        case runUnify (infer init_env tm' >>= prune) (TypingState M.empty tyvar) of
          Left err -> print err
          Right (ty, st) ->
            case evalUnify (eliminateTyVars tm') st of
              Left err -> print err
              Right tm'' -> do
                putStrLn "\nk-normalization -------------------------------------------------"
                let (kn, _) = KN.knormal init_env tm''
                render $ KN.pprint kn

                putStrLn "\nalpha conversion ------------------------------------------------"
                let kn' = AC.alphaConv kn
                render $ KN.pprint kn'

                putStrLn "\nflatten ---------------------------------------------------------"
                let kn'' = KN.flatten kn'
                render $ KN.pprint kn''

                putStrLn "\nclosure conversion ----------------------------------------------"
                let (c, defs) = CC.closureConv kn''
                render $ CC.pprintDecls defs
                putStrLn ""
                render $ CC.pprint c

                putStrLn "\ncode generation -------------------------------------------------"
                render $ Codegen.codegen defs c
