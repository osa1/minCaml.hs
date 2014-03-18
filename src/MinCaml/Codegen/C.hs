{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module MinCaml.Codegen.C where


-------------------------------------------------------------------------------
import           Control.Applicative       hiding (empty)
import           Control.Monad.State
import           Data.List
import qualified Data.Map                  as M
import           Text.PrettyPrint.HughesPJ

import qualified MinCaml.ClosureConv       as CC
import           MinCaml.Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | It turned out that C syntax is very, very complex. So instead, I'm using
-- this custom subset of it.
-- (tried using language-c but couldn't even print a simple function in 30
-- minutes)
data CDecl
    = CFunDecl CType String [(CType, String)] [CStat]
    | CStrDecl String [(String, CType)]
    deriving (Show)

data CType
    = CInt | CFloat | CBool | CPtr CType | CVoidPtr | CFunPtr CType [CType]
    | CTypeName String | CStruct String
    deriving (Show)

data CStat
    = CVarDecl CType String (Maybe CExpr)
    | CAssign String CExpr
    deriving (Show)

data CExpr
    = CVar String
    | CArith CExpr CArithOp CExpr
    deriving (Show)

data CArithOp = CPlus | CMinus
    deriving (Show)


pprintCDecl :: CDecl -> Doc
pprintCDecl (CFunDecl ty fname args body) =
    pprintCType ty <+> text fname <> parens (pprintArgs args) <+> lbrace
    $$ nest 4 (vcat $ map pprintCStat body)
    $$ rbrace
  where
    pprintArgs = sep . punctuate comma . map pprintArg

    pprintArg (CFunPtr ret argTys, vname) =
      pprintCType ret <> parens (char '*' <> text vname) <> parens (pprintTyList argTys)
    pprintArg (ty, name) = pprintCType ty <+> text name
pprintCDecl (CStrDecl name fields) =
    text "typedef struct" <+> lbrace
    $$ nest 4 (pprintFields fields)
    $$ rbrace <+> text name <> semi
  where
    pprintFields = vcat . map pprintField
    pprintField (fieldName, fieldTy) = pprintCType fieldTy <+> text fieldName <> semi


pprintTyList :: [CType] -> Doc
pprintTyList = parens . sep . punctuate comma . map pprintCType


pprintCType :: CType -> Doc
pprintCType CInt = text "int"
pprintCType CFloat = text "float"
pprintCType (CPtr ty) = pprintCType ty <> char '*'
pprintCType (CFunPtr retTy argTys) =
    pprintCType retTy <> parens (char '*') <> pprintTyList argTys
pprintCType (CTypeName name) = text name
pprintCType (CStruct name) = text name
pprintCType CBool = text "bool"
pprintCType CVoidPtr = text "void*"


pprintCStat :: CStat -> Doc
pprintCStat (CVarDecl ty name val) =
    pprintCType ty <+> text name
    <> case val of
         Nothing -> empty
         Just expr -> space <> equals <+> pprintCExpr expr
pprintCStat (CAssign name value) = text name <+> equals <+> pprintCExpr value


pprintCExpr :: CExpr -> Doc
pprintCExpr (CVar var) = text var
pprintCExpr (CArith e1 op e2) = parens $ pprintCExpr e1 <+> pprintCOp op <+> pprintCExpr e2


pprintCOp :: CArithOp -> Doc
pprintCOp CPlus = char '+'
pprintCOp CMinus = char '-'


pprintDecls :: [CDecl] -> Doc
pprintDecls = vcat . intersperse (text "") . map pprintCDecl


test :: Doc
test = pprintDecls decls
  where
    decls :: [CDecl]
    decls = [ CFunDecl CInt "test" [(CInt, "a")] []
            , CFunDecl CInt "test" [(CInt, "a")] []
            ]


main :: IO ()
main = do
    putStrLn $ renderStyle (Style PageMode 90 0.9) test


-------------------------------------------------------------------------------
data CodegenState = CodegenState
    { tupleTys   :: M.Map [Ty] (String, [(String, CType)])
    , lastStruct :: Int
    } deriving (Show)

initCodegenState :: CodegenState
initCodegenState = CodegenState M.empty 0


newtype Codegen a = Codegen { unwrapCodegen :: State CodegenState a }
    deriving (Functor, Applicative, MonadState CodegenState, Monad)


getTupleType :: [Ty] -> Codegen CType
getTupleType tys = do
    tuples <- gets tupleTys
    case M.lookup tys tuples of
      Nothing -> do
        tys' <- mapM genTy tys
        lastS <- gets lastStruct
        let struct = mkStruct lastS tys'
        put (CodegenState (M.insert tys struct tuples) (lastS + 1))
        return $ CStruct $ fst struct
      Just ty -> return $ CStruct $ fst ty
  where
    mkStruct :: Int -> [CType] -> (String, [(String, CType)])
    mkStruct i tys =
      ("tuple" ++ show i
      ,map (\(fi, ty) -> ("tuple" ++ show i ++ "field" ++ show fi, ty))
           (zip [1..] tys))


test' :: String
test' = renderStyle (Style PageMode 90 0.9)
          $ pprintDecls
          $ map (uncurry CStrDecl)
          $ M.elems
          $ tupleTys
          $ execState (unwrapCodegen action) initCodegenState
  where
    action :: Codegen ()
    action = do
      _ <- getTupleType [TyUnit, TyBool, TyFloat]
      _ <- getTupleType [TyUnit, TyBool]
      _ <- getTupleType [TyUnit, TyBool, TyFloat]
      return ()


{-genFunDefs :: [CC.FunDef] -> [CDecl]
genFunDefs = map genFunDef-}


{-genFunDef :: CC.FunDef -> CDecl
genFunDef CC.FunDef{..} =
    CFunDecl (genTy ty) name (genArgs $ fargs ++ fdFvs) (genCC body)-}


genTy :: Ty -> Codegen CType
genTy TyUnit = return CVoidPtr
genTy TyBool = return CBool
genTy TyInt = return CInt
genTy TyFloat = return CFloat
genTy (TyTuple tys) = getTupleType tys
genTy ty = error $ "not implemented: genTy(" ++ show ty ++ ")"


genCC :: CC.CC -> [CStat]
genCC = undefined


genArgs :: [(Id, Ty)] -> [(CType, String)]
genArgs = undefined
