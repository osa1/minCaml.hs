-- | K-normalization
module MinCaml.KNormal
  ( KNormal(..)
  , KFunDef (..)
  , knormal
  , flatten
  , pprint
  , fvs
  ) where

import           Control.Monad.State
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import           Text.PrettyPrint.HughesPJ

import           MinCaml.Types

data KNormal
    = KUnit
    | KInt Int
    | KFloat Float
    | KNeg Id
    | KAdd Id Id
    | KSub Id Id
    | KFNeg Id
    | KFAdd Id Id
    | KFSub Id Id
    | KFMul Id Id
    | KFDiv Id Id
    | KIfEq Id Id KNormal KNormal
    | KIfLe Id Id KNormal KNormal
    | KLet (Id, Ty) KNormal KNormal
    | KVar Id
    | KLetRec KFunDef KNormal
    | KApp Id Ty [Id]
    | KTuple [(Id, Ty)]
    | KLetTuple [(Id, Ty)] Id KNormal
    | KGet Id Id
    | KPut Id Id Id
    | KExtFunApp Id [Id]
    deriving (Show)

data KFunDef = KFunDef (Id, Ty) [(Id, Ty)] KNormal
    deriving (Show)

-- | Free variables
fvs :: KNormal -> S.Set Id
fvs KUnit = S.empty
fvs (KInt _) = S.empty
fvs (KFloat _) = S.empty
fvs (KNeg i) = S.singleton i
fvs (KAdd i1 i2) = S.fromList [i1, i2]
fvs (KSub i1 i2) = S.fromList [i1, i2]
fvs (KFNeg i) = S.singleton i
fvs (KFAdd i1 i2) = S.fromList [i1, i2]
fvs (KFSub i1 i2) = S.fromList [i1, i2]
fvs (KFMul i1 i2) = S.fromList [i1, i2]
fvs (KFDiv i1 i2) = S.fromList [i1, i2]
fvs (KIfEq i1 i2 k1 k2) = S.insert i1 $ S.insert i2 $ fvs k1 `S.union` fvs k2
fvs (KIfLe i1 i2 k1 k2) = S.insert i1 $ S.insert i2 $ fvs k1 `S.union` fvs k2
fvs (KLet (i, _) k1 k2) = fvs k1 `S.union` S.delete i (fvs k2)
fvs (KVar i) = S.singleton i
fvs (KLetRec (KFunDef (i, _) args k1) k2) =
    let k1fvs = S.delete i $ fvs k1 `S.difference` S.fromList (map fst args)
        k2fvs = S.delete i $ fvs k2
    in k1fvs `S.union` k2fvs
fvs (KApp i _ is) = S.insert i $ S.fromList is
fvs (KTuple is) = S.fromList $ map fst is
fvs (KLetTuple is i k) = S.insert i $ fvs k `S.difference` S.fromList (map fst is)
fvs (KGet i1 i2) = S.fromList [i1, i2]
fvs (KPut i1 i2 i3) = S.fromList [i1, i2, i3]
fvs (KExtFunApp _ is) = S.fromList is

freshId :: State Int Id
freshId = do
    i <- get
    put (i + 1)
    return $ "var_k" ++ show i

insertLet :: (KNormal, Ty) -> (Id -> State Int (KNormal, Ty)) -> State Int (KNormal, Ty)
insertLet (KVar i, _) k = k i
insertLet (e, t) k = do
    x <- freshId
    (e', t') <- k x
    return (KLet (x, t) e e', t')

knormal :: M.Map Id Ty -> Tm -> (KNormal, Ty)
knormal env tm = evalState (knormal' env tm) 0

knormal' :: M.Map Id Ty -> Tm -> State Int (KNormal, Ty)
knormal' _ TUnit = return (KUnit, TyUnit)
knormal' _ (TBool b) = return (KInt (if b then 1 else 0), TyInt)
knormal' _ (TInt i) = return (KInt i, TyInt)
knormal' _ (TFloat f) = return (KFloat f, TyFloat)
knormal' env (TNot e) = knormal' env (TIf e (TBool False) (TBool True))
knormal' env (TNeg e) = knormal' env e >>= flip insertLet (\x -> return (KNeg x, TyInt))
knormal' env (TAdd e1 e2) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> return (KAdd x y, TyInt)
knormal' env (TSub e1 e2) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> return (KSub x y, TyInt)
knormal' env (TFNeg e) = do
    ek <- knormal' env e
    insertLet ek $ \x -> return (KFNeg x, TyFloat)
knormal' env (TFAdd e1 e2) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> return (KFAdd x y, TyFloat)
knormal' env (TFSub e1 e2) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> return (KFSub x y, TyFloat)
knormal' env (TFMul e1 e2) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> return (KFMul x y, TyFloat)
knormal' env (TFDiv e1 e2) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> return (KFDiv x y, TyFloat)
knormal' env t@TEq{} = knormal' env (TIf t (TBool True) (TBool False))
knormal' env t@TLE{} = knormal' env (TIf t (TBool True) (TBool False))
knormal' env (TIf (TNot e1) e2 e3) = knormal' env (TIf e1 e3 e2)
knormal' env (TIf (TEq e1 e2) e3 e4) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> do
      (e3', t3) <- knormal' env e3
      (e4', _) <- knormal' env e4
      return (KIfEq x y e3' e4', t3)
knormal' env (TIf (TLE e1 e2) e3 e4) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> do
      (e3', t3) <- knormal' env e3
      (e4', _) <- knormal' env e4
      return (KIfLe x y e3' e4', t3)
knormal' env (TIf e1 e2 e3) = knormal' env (TIf (TEq e1 (TBool False)) e3 e2)
knormal' env (TLet (x, t) e1 e2) = do
    (e1', _) <- knormal' env e1
    (e2', t2) <- knormal' (M.insert x t env) e2
    return (KLet (x, t) e1' e2', t2)
knormal' env (TVar x) =
    case M.lookup x env of
      Nothing -> error $ "unbound var: " ++ x
      Just ty -> return (KVar x, ty)
knormal' env (TLetRec (FunDef (x, t) args e1) e2) = do
    (e1k, _) <- knormal' (M.insert x t (M.fromList args) `M.union` env) e1
    (e2k, e2ty) <- knormal' (M.insert x t env) e2
    return (KLetRec (KFunDef (x, t) args e1k) e2k, e2ty)
knormal' env (TApp f es) = do
    e1k@(_, fty@(TyFun _ retTy)) <- knormal' env f
    insertLet e1k $ \x -> do
      let bind :: [Id] -> [Tm] -> State Int (KNormal, Ty)
          bind argids [] = return (KApp x fty (reverse argids), retTy)
          bind argids (e : es') = do
            ek <- knormal' env e
            insertLet ek $ \i -> bind (i : argids) es'
      bind [] es
knormal' env (TTuple es) = do
    let collectIdTys :: [Id] -> [Ty] -> [Tm] -> State Int (KNormal, Ty)
        collectIdTys is tys [] =
          let tys' = reverse tys
              is' = reverse is
          in return (KTuple (zip is' tys'), TyTuple tys')
        collectIdTys is tys (t : ts) = do
          tk@(_, ty) <- knormal' env t
          insertLet tk $ \i -> collectIdTys (i : is) (ty : tys) ts
    collectIdTys [] [] es
knormal' env (TLetTuple xts e1 e2) = do
    e1k <- knormal' env e1
    (e2k, t2) <- knormal' (M.fromList xts `M.union` env) e2
    insertLet e1k $ \x -> return (KLetTuple xts x e2k, t2)
knormal' env (TArr e1 e2) = do
    e1k <- knormal' env e1
    e2k@(_, ty) <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y ->
      let l = case ty of
                TyFloat -> "create_float_array"
                _ -> "create_array"
      in return (KExtFunApp l [x, y], TyArr ty)
knormal' env (TGet e1 e2) = do
    e1k@(_, TyArr ty) <- knormal' env e1
    e2k <- knormal' env e2
    insertLet e1k $ \x -> insertLet e2k $ \y -> return (KGet x y, ty)
knormal' env (TPut e1 e2 e3) = do
    e1k <- knormal' env e1
    e2k <- knormal' env e2
    e3k <- knormal' env e3
    insertLet e1k $ \x -> insertLet e2k $ \y -> insertLet e3k $ \z -> return (KPut x y z, TyUnit)

-- | Flatten let-bindings
flatten :: KNormal -> KNormal
flatten (KIfEq x y e1 e2) = KIfEq x y (flatten e1) (flatten e2)
flatten (KIfLe x y e1 e2) = KIfLe x y (flatten e1) (flatten e2)
flatten (KLet xt e1 e2) =
    let insert (KLet yt e3 e4) = KLet yt e3 (insert e4)
        insert (KLetRec fundef e) = KLetRec fundef (insert e)
        insert (KLetTuple yts z e) = KLetTuple yts z (insert e)
        insert e = KLet xt e (flatten e2)
    in insert (flatten e1)
flatten (KLetRec (KFunDef name args body) e) = KLetRec (KFunDef name args (flatten body)) (flatten e)
flatten (KLetTuple xts y e) = KLetTuple xts y (flatten e)
flatten e = e

-- | Pretty printer
pprint :: KNormal -> Doc
pprint KUnit = text "()"
pprint (KInt i) = int i
pprint (KFloat f) = float f
pprint (KNeg x) = parens $ char '-' <> text x
pprint (KAdd i1 i2) = parens $ text i1 <+> char '+' <+> text i2
pprint (KSub i1 i2) = parens $ text i1 <+> char '-' <+> text i2
pprint (KFNeg x) = parens $ text "-." <> text x
pprint (KFAdd i1 i2) = parens $ text i1 <+> text "+." <+> text i2
pprint (KFSub i1 i2) = parens $ text i1 <+> text "-." <+> text i2
pprint (KFMul i1 i2) = parens $ text i1 <+> text "*." <+> text i2
pprint (KFDiv i1 i2) = parens $ text i1 <+> text "/." <+> text i2
pprint (KIfEq i1 i2 k1 k2) = text "if" <+> text i1 <+> char '=' <+> text i2 <+> text "then"
                               $$ nest 4 (pprint k1)
                               $$ text "else"
                               $$ nest 4 (pprint k2)
pprint (KIfLe i1 i2 k1 k2) = text "if" <+> text i1 <+> char '<' <+> text i2 <+> text "then"
                               $$ nest 4 (pprint k1)
                               $$ text "else"
                               $$ nest 4 (pprint k2)
pprint (KLet (x, t) k1 k2) = sep [ hang (text "let" <+> text x <> char ':' <+> text (show t) <+> char '=')
                                        4 (pprint k1)
                                 , text "in"
                                 ] $$ pprint k2
pprint (KVar x) = text x
pprint (KLetRec fundef k) = sep [ pprintFunDef fundef
                                , text "in"
                                ] $$ pprint k
pprint (KApp x _ args) = text x <+> hsep (map text args)
pprint (KTuple idtys) = parens $ hcat $
    punctuate (text ", ") (map (\(i, t) -> text i <+> char ':' <+> text (show t)) idtys)
pprint (KLetTuple xts i k) =
    sep [ text "let" <+> parens (pprintArgs xts) <+> char '=' <+> text i
        , text "in"
        ] $$ pprint k
pprint (KGet i1 i2) = parens (text i1) <> char '.' <> text i2
pprint (KPut i1 i2 i3) = parens (text i1) <> char '.' <> text i2 <+> text "<-" <+> text i3
pprint (KExtFunApp x args) = char '#' <> text x <+> hsep (map text args)

pprintArgs :: [(Id, Ty)] -> Doc
pprintArgs = sep . punctuate (char ',') . map (\(x, t) -> text x <> text ": " <> text (show t))

pprintFunDef :: KFunDef -> Doc
pprintFunDef (KFunDef (x, t) args body) =
    hang (text "let rec" <+> text x <> char ':' <+> text (show t)
                    <+> parens (pprintArgs args)
                    <+> char '=')
         2 (pprint body)

