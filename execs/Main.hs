module Main where

import           Control.Monad.Except
import           Data.Either               (either)
import qualified Data.Map                  as M
import           Safe
import           System.Environment
import           System.FilePath           (takeBaseName)
import           Text.PrettyPrint.HughesPJ hiding (render)

import           LLVM.General.Context
import qualified LLVM.General.Module       as LLVM

import qualified MinCaml.AlphaConv         as AC
import qualified MinCaml.ClosureConv       as CC
import           MinCaml.Codegen.LLVM      (codegen)
import qualified MinCaml.KNormal           as KN
import qualified MinCaml.Parser            as P
import           MinCaml.Types
import           MinCaml.Typing

render :: Doc -> IO ()
render = putStrLn . renderStyle (Style PageMode 80 0.9)

-- TODO: remove these horrible wrappers sometime and refactor stuff properly

mEitherM :: Monad m => m (Either a b) -> ExceptT a m b
mEitherM e = either throwError return =<< lift e

makeEM :: Monad m => Either a b -> ExceptT a m b
makeEM = either throwError return

errS :: (Monad m, Show err) => ExceptT err m b -> ExceptT String m b
errS e = either (throwError . show) return =<< lift (runExceptT e)

main :: IO ()
main = do
    args <- getArgs
    ret  <- runExceptT $ do
      target <- maybe (throwError "provide a file") return $ atMay args 0
      (tm, tyvar) <- mEitherM $ P.parseFile' target
      (ty, st) <- errS . makeEM $ runUnify (infer init_env tm >>= prune) (TypingState M.empty tyvar)
      tm' <- errS . makeEM $ evalUnify (eliminateTyVars tm) st

      liftIO $ putStrLn "\nk-normalization -------------------------------------------------"
      let (kn, _) = KN.knormal init_env tm'
      liftIO $ render $ KN.pprint kn

      liftIO $ putStrLn "\nalpha conversion ------------------------------------------------"
      let kn' = AC.alphaConv kn
      liftIO $ render $ KN.pprint kn'

      liftIO $ putStrLn "\nflatten ---------------------------------------------------------"
      let kn'' = KN.flatten kn'
      liftIO $ render $ KN.pprint kn''

      liftIO $ putStrLn "\nclosure conversion ----------------------------------------------"
      let (c, defs) = CC.closureConv kn''
      liftIO $ do
        render $ CC.pprintDecls defs
        putStrLn ""
        render $ CC.pprint c

      liftIO $ putStrLn "\ncode generation -------------------------------------------------"
      liftIO $ withContext $ \ctx ->
        runExceptT $ LLVM.withModuleFromAST ctx (codegen (takeBaseName target) defs c) $ \m -> do
          liftIO $ putStrLn =<< LLVM.moduleLLVMAssembly m

    either putStrLn (const $ return ()) ret

