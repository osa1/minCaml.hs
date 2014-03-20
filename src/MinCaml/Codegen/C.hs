{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module MinCaml.Codegen.C where


-------------------------------------------------------------------------------
import           Control.Applicative       hiding (empty)
import           Control.Monad.State
import           Data.List
import qualified Data.Map                  as M
import           Text.PrettyPrint.HughesPJ

import qualified MinCaml.ClosureConv       as CC
import           MinCaml.Types             (Id, Ty (..))
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | It turned out that C syntax is very, very complex. So instead, I'm using
-- this custom subset of it.
-- (tried using language-c but couldn't even print a simple function in 30
-- minutes)
data CDecl
    = CFunDecl CType String [(CType, String)] (Maybe [CStat])
    | CStrDecl String [(String, CType)]
    deriving (Show)

data CType
    = CInt | CFloat | CBool | CPtr CType | CVoidPtr | CFunPtr CType [CType]
    | CTypeName String | CStruct String
    deriving (Show)

data CStat
    = CVarDecl CType String (Maybe CExpr)
    | CAssign String CExpr
    | CIf CExpr [CStat] [(CExpr, [CStat])]
    | CFunCall String [CExpr]
    | CReturn CExpr
    deriving (Show)

data CExpr
    = CVar String
    | CTrue | CFalse
    | CArith CExpr CArithOp CExpr
    | CFunCallE String [CExpr]
    | CIntE Int | CFloatE Float
    deriving (Show)

data CArithOp = CPlus | CMinus | CEqual | CLe
    deriving (Show)


pprintCDecl :: CDecl -> Doc
pprintCDecl (CFunDecl ty fname args body) =
    pprintCType ty <+> text fname <> parens (pprintArgs args)
    $+$ case body of
          Nothing -> empty
          Just body' -> pprintBlock body'
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
pprintCStat (CVarDecl (CFunPtr retTy argTys) name val) =
    pprintCType retTy <> parens (char '*' <> text name) <> pprintTyList argTys
    <> case val of
         Nothing -> empty
         Just expr -> space <> equals <+> pprintCExpr expr
pprintCStat (CVarDecl ty name val) =
    pprintCType ty <+> text name
    <> case val of
         Nothing -> empty
         Just expr -> space <> equals <+> pprintCExpr expr
pprintCStat (CAssign name value) = text name <+> equals <+> pprintCExpr value
pprintCStat (CIf guard thenCase alts) = pprintCases (zip guards bodies)
  where
    guards = guard : map fst alts
    bodies = thenCase : map snd alts

    pprintCases ((g, b) : rest) =
      text "if" <+> parens (pprintCExpr g) <+> lbrace $+$ nest 4 (pprintBlock' b)
        $+$ rbrace <+> pprintCases' rest

    pprintCases' [] = empty
    pprintCases' ((g, b) : rest) =
      text "else if" <+> parens (pprintCExpr g) <+> lbrace $+$ nest 2 (pprintBlock' b)
        $+$ nest (-2) (rbrace <+> pprintCases' rest)
pprintCStat (CFunCall fname args) = pprintFunCall fname args
pprintCStat (CReturn expr) = text "return" <+> pprintCExpr expr


pprintBlock :: [CStat] -> Doc
pprintBlock block = lbrace $+$ nest 4 (vcat $ map pprintCStat block) $+$ rbrace


pprintBlock' :: [CStat] -> Doc
pprintBlock' block = vcat $ map pprintCStat block


pprintCExpr :: CExpr -> Doc
pprintCExpr (CVar var) = text var
pprintCExpr (CArith e1 op e2) = parens $ pprintCExpr e1 <+> pprintCOp op <+> pprintCExpr e2
pprintCExpr CTrue = text "true"
pprintCExpr CFalse = text "false"
pprintCExpr (CFunCallE fname args) = pprintFunCall fname args
pprintCExpr (CIntE i) = int i
pprintCExpr (CFloatE f) = float f


pprintFunCall :: String -> [CExpr] -> Doc
pprintFunCall fname args = text fname <> parens (sep $ punctuate comma $  map pprintCExpr args)


pprintCOp :: CArithOp -> Doc
pprintCOp CPlus = char '+'
pprintCOp CMinus = char '-'
pprintCOp CEqual = text "=="
pprintCOp CLe = text "<="


pprintDecls :: [CDecl] -> Doc
pprintDecls = vcat . intersperse (text "") . map pprintCDecl


test :: Doc
test = pprintDecls decls
  where
    decls :: [CDecl]
    decls = [ CFunDecl CInt "test" [(CInt, "a")] Nothing
            , CFunDecl CInt "test" [(CInt, "a")] Nothing
            ]


main :: IO ()
main = do
    putStrLn $ renderStyle (Style PageMode 90 0.9) test


-------------------------------------------------------------------------------
data CodegenState = CodegenState
    { -- states used for generating structs for tuples
      tupleTys    :: M.Map [Ty] (String, [(String, CType)])
    , lastTStruct :: Int
      -- states used for generating structs for closures
    , closureTys  :: M.Map [Ty] (String, [(String, CType)])
    , lastCStruct :: Int

    , closures    :: M.Map Id CC.FunDef
    , freshVar    :: Int
    } deriving (Show)

initCodegenState :: M.Map Id CC.FunDef -> CodegenState
initCodegenState closures = CodegenState M.empty 0 M.empty 0 cls 0
  where
    cls = closures `M.union` builtins
    builtins = M.fromList
      [ ("print_int", CC.FunDef "print_int" (TyFun [TyInt] TyUnit) [("i", TyInt)] [] Nothing) ]


newtype Codegen a = Codegen { unwrapCodegen :: State CodegenState a }
    deriving (Functor, Applicative, MonadState CodegenState, Monad)


getTupleTy :: [Ty] -> Codegen CType
getTupleTy tys = do
    st <- get
    let tuples = tupleTys st
    case M.lookup tys tuples of
      Nothing -> do
        tys' <- mapM genTy tys
        let lastS = lastTStruct st
        let struct = mkStruct lastS tys'
        put st{tupleTys=M.insert tys struct tuples, lastTStruct=(lastS + 1)}
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
          $ execState (unwrapCodegen action) (initCodegenState M.empty)
  where
    action :: Codegen ()
    action = do
      _ <- getTupleTy [TyUnit, TyBool, TyFloat]
      _ <- getTupleTy [TyUnit, TyBool]
      _ <- getTupleTy [TyUnit, TyBool, TyFloat]
      return ()


genTy :: Ty -> Codegen CType
genTy TyUnit = return CVoidPtr
genTy TyBool = return CBool
genTy TyInt = return CInt
genTy TyFloat = return CFloat
genTy (TyTuple tys) = getTupleTy tys
genTy (TyArr ty) = CPtr <$> genTy ty
genTy (TyFun argtys retty) = CFunPtr <$> genTy retty <*> mapM genTy argtys
genTy (TyVar tyvar) = error $ "uninstantiated type variable in genTy: " ++ show tyvar


getBinder :: Maybe Id -> Codegen Id
getBinder (Just i) = return i
getBinder Nothing = do
    st <- get
    let ret = freshVar st
    put st{freshVar=ret+1}
    return ("var_cg$" ++ show ret)


genCC
    :: Maybe Id         -- ^ assign last expression to a variable
    -> CC.CC            -- ^ closure converted minCaml syntax
    -> Codegen [CStat]  -- ^ generated C statements
genCC asgn CC.CUnit = do
    var <- getBinder asgn
    return [CAssign var (CIntE 0)]
genCC asgn (CC.CInt i) = do
    var <- getBinder asgn
    return [CAssign var (CIntE i)]
genCC asgn (CC.CFloat f) = do
    var <- getBinder asgn
    return [CAssign var (CFloatE f)]
genCC asgn (CC.CAdd i1 i2) = do
    var <- getBinder asgn
    return [CAssign var (CArith (CVar i1) CPlus (CVar i2))]
genCC asgn (CC.CSub i1 i2) = do
    var <- getBinder asgn
    return [CAssign var (CArith (CVar i1) CMinus (CVar i2))]
genCC asgn (CC.CVar i) = do
    var <- getBinder asgn
    return [CAssign var (CVar i)]
genCC asgn (CC.CIfEq i1 i2 c1 c2) = do
    c1' <- genCC asgn c1
    c2' <- genCC asgn c2
    return [CIf (CArith (CVar i1) CEqual (CVar i2))
                c1'
                [(CTrue, c2')]]
genCC asgn (CC.CIfLe i1 i2 c1 c2) = do
    c1' <- genCC asgn c1
    c2' <- genCC asgn c2
    return [CIf (CArith (CVar i1) CLe (CVar i2))
                c1'
                [(CTrue, c2')]]
genCC asgn (CC.CLet (i, t) c1 c2) = do
    t' <- genTy t
    let decl = CVarDecl t' i Nothing
    declStat <- genCC (Just i) c1
    rest <- genCC asgn c2
    return (decl : declStat ++ rest)

genCC asgn (CC.CMkCls (fname, _) closure rest) = do
    -- structName <- genCls closure
    genCC asgn rest

genCC asgn (CC.CAppCls fname args) = do
    cls <- gets closures
    case M.lookup fname cls of
      Nothing -> do
        let as = map CVar args
        return $ case asgn of
                   Nothing -> [CFunCall fname as]
                   Just aid -> [CAssign aid (CFunCallE fname as)]
      Just CC.FunDef{..} -> do
        let as = map CVar $ args ++ map fst fdFvs
        return $ case asgn of
                   Nothing -> [CFunCall name as]
                   Just aid -> [CAssign aid (CFunCallE name as)]
genCC asgn (CC.CAppDir fname args) = do
    let as = map CVar args
    return $ case asgn of
               Nothing -> [CFunCall fname as]
               Just aid -> [CAssign aid (CFunCallE fname as)]
genCC _ cc = error $ "not implemented yet: genCC " ++ show cc


genFunDefs :: [CC.FunDef] -> Codegen [CDecl]
genFunDefs = mapM genFunDef


genFunDef :: CC.FunDef -> Codegen CDecl
genFunDef CC.FunDef{..} = do
    retty <- genTy ty
    argTys <- mapM genTy (map snd fargs)
    fdFvsTys <- mapM genTy (map snd fdFvs)
    let retName = "var_cg$funret"
    body' <- maybe (return Nothing) (liftM Just . genCC (Just retName)) body
    return $ CFunDecl retty name (zip argTys (map fst fargs) ++ zip fdFvsTys (map fst fdFvs))
                      (fmap (++ [CReturn (CVar retName)]) body')


-------------------------------------------------------------------------------
codegen :: M.Map Id CC.FunDef -> CC.CC -> Doc
codegen fundefs code =
    let (defs, c) =
          evalState (unwrapCodegen ((,) <$> genFunDefs (M.elems fundefs)
                                        <*> genCC Nothing code))
                    (initCodegenState fundefs)
    in pprintDecls defs
       $$ empty
       $$ (mkMain $ pprintBlock c)
  where
    mkMain c = text "int" <+> text "main" <> parens empty $+$ c


