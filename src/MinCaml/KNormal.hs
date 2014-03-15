
-- | K-normalization
module MinCaml.KNormal where


import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Set            as S

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
    | KApp Id [Id]
    | KTuple [Id]
    | KLetTuple [(Id, Ty)] Id KNormal
    | KGet Id Id
    | KPut Id Id Id
    | KExtArray Id
    | KExtFunApp Id [Id]
    deriving (Show)

data KFunDef = KFunDef (Id, Ty) [(Id, Ty)] KNormal
    deriving (Show)


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
fvs (KApp i is) = S.insert i $ S.fromList is
fvs (KTuple is) = S.fromList is
fvs (KLetTuple is i k) = S.insert i $ fvs k `S.difference` S.fromList (map fst is)
fvs (KGet i1 i2) = S.fromList [i1, i2]
fvs (KPut i1 i2 i3) = S.fromList [i1, i2, i3]
fvs (KExtArray _) = S.empty
fvs (KExtFunApp _ is) = S.fromList is


-------------------------------------------------------------------------------
freshId :: State Int Id
freshId = do
    i <- get
    put (i + 1)
    return $ "var_k$" ++ show i


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
    e1k@(_, TyFun _ retTy) <- knormal' env f
    insertLet e1k $ \x -> do
      let bind :: [Id] -> [Tm] -> State Int (KNormal, Ty)
          bind argids [] = return (KApp x (reverse argids), retTy)
          bind argids (e : es') = do
            ek <- knormal' env e
            insertLet ek $ \i -> bind (i : argids) es'
      bind [] es
knormal' env (TTuple es) = do
    let collectIdTys :: [Id] -> [Ty] -> [Tm] -> State Int (KNormal, Ty)
        collectIdTys is tys [] = return (KTuple $ reverse is, TyTuple $ reverse tys)
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
