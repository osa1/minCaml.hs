{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module MinCaml.Codegen.C where


-------------------------------------------------------------------------------
import           Control.Applicative       hiding (empty)
import           Control.Monad.State
import           Data.List
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import           Text.PrettyPrint.HughesPJ

import qualified MinCaml.ClosureConv       as CC
import           MinCaml.Types             (Id, Ty (..), isFunTy)

import           Debug.Trace
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
    deriving (Show, Eq, Ord)

data CStat
    = CVarDecl CType String (Maybe CExpr)
    | CAssign String CExpr
    | CIf CExpr [CStat] [(CExpr, [CStat])]
    | CFunCall CExpr [CExpr]
    | CReturn CExpr
    deriving (Show)

data CExpr
    = CVar String
    | CTrue | CFalse
    | CArith CExpr CArithOp CExpr
    | CFunCallE CExpr [CExpr]
    | CIntE Int | CFloatE Float
    | CSelect CExpr String
    | CStructE [CExpr]
    deriving (Show)

data CArithOp = CPlus | CMinus | CMult | CEqual | CLe
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
    <> (case val of
          Nothing -> empty
          Just expr -> space <> equals <+> pprintCExpr expr)
    <> semi
pprintCStat (CVarDecl ty name val) =
    pprintCType ty <+> text name
    <> (case val of
          Nothing -> empty
          Just expr -> space <> equals <+> pprintCExpr expr)
    <> semi
pprintCStat (CAssign name value) = text name <+> equals <+> pprintCExpr value <> semi
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
pprintCStat (CFunCall fn args) = pprintFunCall fn args <> semi
pprintCStat (CReturn expr) = text "return" <+> pprintCExpr expr <> semi


pprintBlock :: [CStat] -> Doc
pprintBlock block = lbrace $+$ nest 4 (vcat $ map pprintCStat block) $+$ rbrace


pprintBlock' :: [CStat] -> Doc
pprintBlock' block = vcat $ map pprintCStat block


pprintCExpr :: CExpr -> Doc
pprintCExpr (CVar var) = text var
pprintCExpr (CArith e1 op e2) = parens $ pprintCExpr e1 <+> pprintCOp op <+> pprintCExpr e2
pprintCExpr CTrue = text "true"
pprintCExpr CFalse = text "false"
pprintCExpr (CFunCallE fn args) = pprintFunCall fn args
pprintCExpr (CIntE i) = int i
pprintCExpr (CFloatE f) = float f
pprintCExpr (CSelect e s) = parens (pprintCExpr e) <> char '.' <> text s
pprintCExpr (CStructE es) = braces (sep $ punctuate comma $ map pprintCExpr es)


pprintFunCall :: CExpr -> [CExpr] -> Doc
pprintFunCall fn args = parens (pprintCExpr fn) <> parens (sep $ punctuate comma $ map pprintCExpr args)


pprintCOp :: CArithOp -> Doc
pprintCOp CPlus = char '+'
pprintCOp CMinus = char '-'
pprintCOp CEqual = text "=="
pprintCOp CLe = text "<="
pprintCOp CMult = char '*'


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
main =
    putStrLn $ renderStyle (Style PageMode 90 0.9) test


-------------------------------------------------------------------------------
data CodegenState = CodegenState
    { -- states used for generating structs for tuples
      tupleTys      :: M.Map [Ty] (String, [(String, CType)])
    , lastTStruct   :: Int

      -- states used for generating structs for closures and closure envs
    , closureTys    :: M.Map Ty String
    , envTys        :: M.Map [(Id, Ty)] String
    , lastClosureTy :: Int
    , lastEnvTy     :: Int

    , freshVar      :: Int
    } deriving (Show)

initCodegenState :: M.Map Id CC.FunDef -> CodegenState
initCodegenState closures = CodegenState M.empty 0 M.empty M.empty 0 0 0
  where
    cls = closures `M.union` builtins
    builtins = M.fromList
      [ ("print_int", CC.FunDef "print_int" (TyFun [TyInt] TyUnit) [("i", TyInt)] M.empty Nothing) ]


newtype Codegen a = Codegen { unwrapCodegen :: State CodegenState a }
    deriving (Functor, Applicative, MonadState CodegenState, Monad)


getTupleStructName :: [Ty] -> Codegen String
getTupleStructName tys = do
    st <- get
    let tuples = tupleTys st
    case M.lookup tys tuples of
      Nothing -> do
        tys' <- mapM genTy tys
        let lastS = lastTStruct st
        let struct = mkStruct lastS tys'
        put st{tupleTys=M.insert tys struct tuples, lastTStruct=lastS + 1}
        return $ fst struct
      Just struct -> return $ fst struct
  where
    mkStruct :: Int -> [CType] -> (String, [(String, CType)])
    mkStruct i tys =
      ("tuple" ++ show i
      ,map (\(fi, ty) -> ("tuple" ++ show i ++ "field" ++ show fi, ty))
           (zip [1..] tys))


getClosureStructName
    :: Ty                       -- ^ closure function type
    -> Codegen String           -- ^ struct name for closure
getClosureStructName funty = do
    st <- gets closureTys
    case M.lookup funty st of
      Nothing -> do
        cty <- gets lastClosureTy
        let cname = "closure" ++ show cty
        modify $ \s -> s{lastClosureTy = cty + 1, closureTys = M.insert funty cname st}
        -- TODO: generate struct code
        return cname
      Just n -> return n


getEnvStructName
    :: [(Id, Ty)]           -- ^ free variables and their types
    -> Codegen String       -- ^ struct name for environment
getEnvStructName fvs = do
    st <- gets envTys
    case M.lookup fvs st of
      Nothing -> do
        ety <- gets lastEnvTy
        let envname = "env" ++ show ety
        modify $ \s -> s{lastEnvTy = ety + 1, envTys = M.insert fvs envname st}
        -- TODO: generate struct code
        return envname
      Just n -> return n


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
      _ <- getTupleStructName [TyUnit, TyBool, TyFloat]
      _ <- getTupleStructName [TyUnit, TyBool]
      _ <- getTupleStructName [TyUnit, TyBool, TyFloat]
      return ()


genTy :: Ty -> Codegen CType
genTy TyUnit = return CVoidPtr
genTy TyBool = return CBool
genTy TyInt = return CInt
genTy TyFloat = return CFloat
genTy (TyTuple tys) = CTypeName <$> getTupleStructName tys
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
genCC asgn (CC.CNeg i) = do
    var <- getBinder asgn
    return [CAssign var (CArith (CIntE (-1)) CMult (CVar i))]
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
    t' <- if isFunTy t then
            CStruct <$> getClosureStructName t
          else
            genTy t
    let decl = CVarDecl t' i Nothing
    declStat <- genCC (Just i) c1
    rest <- genCC asgn c2
    return (decl : declStat ++ rest)

-- Compiling tuples
genCC asgn (CC.CTuple idtys) = do
    let tys = map snd idtys
        ids = map fst idtys
    structName <- getTupleStructName tys
    var <- getBinder asgn
    return [CAssign var $ CStructE $ map CVar ids]
genCC asgn (CC.CLetTuple binders i c) = do
    structName <- getTupleStructName (map snd binders)
    binderTys <- mapM (genTy . snd) binders
    let binderIds = map fst binders
        binds = map (\(fi, bi, t) -> CVarDecl t bi (Just $ CSelect (CVar i) (structName ++ "field" ++ show fi)))
                    (zip3 [1..] binderIds binderTys)
    rest <- genCC asgn c
    return $ binds ++ rest

-- Compiling closures and closure applications
genCC asgn (CC.CMkCls (var, ty) (CC.Closure entry fvs) rest) = do
    clsname <- getClosureStructName ty
    envname <- getEnvStructName (M.toList fvs)
    envvar <- getBinder Nothing
    let envStruct = CVarDecl (CTypeName envname) envvar (Just $ CStructE $ map (CVar . fst) $ M.toList fvs)
        funStruct = CVarDecl (CTypeName clsname) var (Just $ CStructE [CVar $ var ++ "_fun" ,CVar envvar])
    rest' <- genCC asgn rest
    return $ envStruct : funStruct : rest'

genCC asgn (CC.CAppCls fname args) = genCC asgn (CC.CAppDir fname args)

genCC asgn (CC.CAppDir fname args) = do
    let as = map CVar args
    return $ case asgn of
               Nothing -> [CFunCall (CVar fname) as]
               Just aid -> [CAssign aid (CFunCallE (CVar fname) as)]
genCC _ cc = error $ "not implemented yet: genCC " ++ show cc


genFunDefs :: [CC.FunDef] -> Codegen [CDecl]
genFunDefs = mapM genFunDef


genFunDef :: CC.FunDef -> Codegen CDecl
genFunDef CC.FunDef{..} = do
    retty <- do
      let rt = getFunRetTy ty
      if isFunTy rt then do
        closureName <- getClosureStructName rt
        return $ CTypeName closureName
      else
        genTy rt
    argTys <- mapM (genTy . snd) fargs
    fdFvsTys <- mapM (genTy . snd) (M.toList fdFvs)
    body' <- maybe (return Nothing) (liftM Just . genCC (Just retName)) body
    return $ CFunDecl retty name (zip argTys (map fst fargs) ++ zip fdFvsTys (M.keys fdFvs))
                      (addRetStat retty body')
  where
    getFunRetTy :: Ty -> Ty
    getFunRetTy (TyFun _ retty) = retty
    getFunRetTy ty = error $ "panic: function has non-function type " ++ show ty

    retName = "var_cg$funret"

    funret :: CType -> CStat
    funret retty = CVarDecl retty retName Nothing

    addRetStat :: CType -> Maybe [CStat] -> Maybe [CStat]
    addRetStat _ Nothing = Nothing
    addRetStat retty (Just b) = Just $ funret retty : b ++ [CReturn (CVar retName)]


-------------------------------------------------------------------------------
codegen :: M.Map Id CC.FunDef -> CC.CC -> Doc
codegen fundefs code =
    let (defs, c) =
          evalState (unwrapCodegen ((,) <$> genFunDefs (M.elems fundefs)
                                        <*> genCC Nothing code))
                    (initCodegenState fundefs)
    in pprintDecls defs
       $$ empty
       $$ mkMain (pprintBlock c)
  where
    mkMain c = text "int" <+> text "main" <> parens empty $+$ c


