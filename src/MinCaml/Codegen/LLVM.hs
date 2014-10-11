module MinCaml.Codegen.LLVM where

import qualified Data.Map            as M

import           LLVM.General.AST

import qualified MinCaml.ClosureConv as CC
import           MinCaml.Types       (Id, Ty (..), isFunTy)

codegen :: String -> M.Map Id CC.FunDef -> CC.CC -> Module
codegen mname funs code = undefined

