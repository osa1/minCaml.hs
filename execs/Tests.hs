
module Main where


import           Data.List
import qualified Data.Map         as M
import           Data.Ord
import           System.Directory
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit

import           MinCaml.Parser
import           MinCaml.Types
import           MinCaml.Typing


main :: IO ()
main = do
    mlFiles <- readMlFiles
    defaultMain (testGroup "unit tests" [tcTests mlFiles])


readMlFiles :: IO [(String, String)]
readMlFiles = do
    files <- fmap (filter ((== ".ml") . takeExtension)) $ getDirectoryContents "min-caml/test"
    contents <- mapM (readFile . ("min-caml/test" </>)) files
    return $ zip files contents


tcTests :: [(String, String)] -> TestTree
tcTests files = testGroup "type checker tests" $
    map (uncurry mkTestCase) files
  where
    init_env = M.fromList
      [ ("print_int", TyFun [TyInt] TyUnit)
      , ("truncate", TyFun [TyFloat] TyInt)
      , ("int_of_float", TyFun [TyFloat] TyInt)
      , ("float_of_int", TyFun [TyInt] TyFloat)
      , ("abs_float", TyFun [TyFloat] TyFloat)
      , ("sqrt", TyFun [TyFloat] TyFloat)
      , ("cos", TyFun [TyFloat] TyFloat)
      , ("sin", TyFun [TyFloat] TyFloat)
      , ("print_newline", TyFun [TyUnit] TyUnit)
      ]

    mkTestCase fname contents = testCase fname $
      let tm = parseStr' contents in
      case tm of
        Left err -> error $ "parse error: " ++ err
        Right (tm', tyvar) -> do
          assertEqual "" (Right TyUnit) $
            evalUnify (infer init_env tm' >>= prune) (TypingState M.empty tyvar)
