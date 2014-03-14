{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Typing where


-------------------------------------------------------------------------------
import           Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Map               as M
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data TypingState = TypingState
    { unifications_ :: M.Map TyVar Ty
    , freshTyVar_   :: TyVar
    } deriving (Show)


data UnificationError
    = OccursCheck Ty Ty
    | UnificationError Ty Ty
    | UnboundVar Id
    | StrErr String
    deriving (Show, Eq)

instance Error UnificationError where
    strMsg = StrErr


newtype Unify a = Unify { unwrapUnify :: StateT TypingState (ErrorT UnificationError Identity) a }
    deriving (Functor, Applicative, Monad, MonadState TypingState, MonadError UnificationError)


evalUnify :: Unify a -> TypingState -> Either UnificationError a
evalUnify a s = runIdentity $ runErrorT $ evalStateT (unwrapUnify a) s


runUnify :: Unify a -> TypingState -> Either UnificationError (a, TypingState)
runUnify a s = runIdentity $ runErrorT $ runStateT (unwrapUnify a) s


lookupTyvar :: TyVar -> Unify (Maybe Ty)
lookupTyvar tyvar = do
    us <- gets unifications_
    return $ M.lookup tyvar us


bindTyvar :: TyVar -> Ty -> Unify ()
bindTyvar var ty =
    modify (\s@TypingState{unifications_=us} -> s{unifications_=M.insert var ty us})


freshTyVar :: Unify TyVar
freshTyVar = do
    s@TypingState{freshTyVar_=ftv} <- get
    put s{freshTyVar_=ftv+1}
    return ftv


-------------------------------------------------------------------------------
infer :: M.Map Id Ty -> Tm -> Unify Ty
infer _ TUnit = return TyUnit
infer _ TBool{} = return TyBool
infer _ TInt{} = return TyInt
infer _ TFloat{} = return TyFloat
infer env (TNot e) = do
    unifyExp env e TyBool
    return TyBool
infer env (TNeg e) = do
    unifyExp env e TyInt
    return TyInt
infer env (TAdd e1 e2) = do
    unifyExp env e1 TyInt
    unifyExp env e2 TyInt
    return TyInt
infer env (TSub e1 e2) = do
    unifyExp env e1 TyInt
    unifyExp env e2 TyInt
    return TyInt
infer env (TFNeg e) = do
    unifyExp env e TyFloat
    return TyFloat
infer env (TFAdd e1 e2) = do
    unifyExp env e1 TyFloat
    unifyExp env e2 TyFloat
    return TyFloat
infer env (TFSub e1 e2) = do
    unifyExp env e1 TyFloat
    unifyExp env e2 TyFloat
    return TyFloat
infer env (TFMul e1 e2) = do
    unifyExp env e1 TyFloat
    unifyExp env e2 TyFloat
    return TyFloat
infer env (TFDiv e1 e2) = do
    unifyExp env e1 TyFloat
    unifyExp env e2 TyFloat
    return TyFloat
infer env (TLE e1 e2) = do
    e1ty <- infer env e1
    unifyExp env e2 e1ty
    return TyBool
infer env (TEq e1 e2) = do
    e1ty <- infer env e1
    unifyExp env e2 e1ty
    return TyBool
infer env (TIf e1 e2 e3) = do
    unifyExp env e1 TyBool
    e2ty <- infer env e2
    unifyExp env e3 e2ty
    return e2ty
infer env (TLet (id, ty) e1 e2) = do
    unifyExp env e1 ty
    infer (M.insert id ty env) e2
infer env (TVar var) =
    case M.lookup var env of
      Nothing -> throwError $ UnboundVar var
      Just ty -> return ty
infer env (TLetRec (FunDef (id, ty) args e1) e2) = do
    let env' = M.insert id ty env
    bodyTy <- infer (addTys args env') e1
    unify ty (TyFun (map snd args) bodyTy)
    infer env' e2
infer env (TApp tm args) = do
    ret <- TyVar <$> freshTyVar
    argTys <- mapM (infer env) args
    unifyExp env tm (TyFun argTys ret)
    return ret
infer env (TTuple tms) = TyTuple <$> mapM (infer env) tms
infer env (TLetTuple xts e1 e2) = do
    unifyExp env e1 (TyTuple $ map snd xts)
    infer (addTys xts env) e2
infer env (TArr e1 e2) = do
    unifyExp env e1 TyInt
    TyArr <$> infer env e2
infer env (TGet e1 e2) = do
    ftv <- TyVar <$> freshTyVar
    unifyExp env e1 (TyArr ftv)
    unifyExp env e2 TyInt
    return ftv
infer env (TPut e1 e2 e3) = do
    e3ty <- infer env e3
    unifyExp env e1 e3ty
    unifyExp env e2 TyInt
    return TyUnit


addTys :: [(Id, Ty)] -> M.Map Id Ty -> M.Map Id Ty
addTys args env = foldl (flip $ uncurry M.insert) env args


unifyExp :: M.Map Id Ty -> Tm -> Ty -> Unify ()
unifyExp env tm ty = do
    expty <- infer env tm
    unify expty ty


unify :: Ty -> Ty -> Unify ()
unify ty1 ty2 = do
    ty1' <- prune ty1
    ty2' <- prune ty2
    case (ty1', ty2') of
      (TyUnit, TyUnit) -> return ()
      (TyBool, TyBool) -> return ()
      (TyInt, TyInt) -> return ()
      (TyFloat, TyFloat) -> return ()
      (TyFun args1 body1, TyFun args2 body2) -> do
        zipWithM_ unify args1 args2
        unify body1 body2
      (TyTuple t1s, TyTuple t2s) -> zipWithM_ unify t1s t2s
      (TyArr t1, TyArr t2) -> unify t1 t2
      (TyVar var1, TyVar var2) | var1 == var2 -> return ()
      (TyVar var1, _) -> bindTyvar var1 ty2'
      (_, TyVar var2) -> bindTyvar var2 ty1'
      _ -> throwError $ UnificationError ty1' ty2'


occursCheck :: Ty -> Ty -> Unify Bool
occursCheck ty1 ty2 =
    case ty2 of
      TyFun tys ty -> do
        r1 <- foldr (||) False <$> mapM (occursCheck ty1) tys
        r2 <- occursCheck ty1 ty
        return (r1 || r2)
      TyTuple tys -> foldl (||) False <$> mapM (occursCheck ty1) tys
      TyArr ty -> occursCheck ty1 ty
      TyVar tyvar
        | ty1 == ty2 -> return True
        | otherwise  -> do
            ty <- lookupTyvar tyvar
            case ty of
              Nothing -> return False
              Just ty' -> occursCheck ty1 ty'
      _ -> return False


prune :: Ty -> Unify Ty
prune (TyVar tyvar) = do
    tyvar' <- lookupTyvar tyvar
    case tyvar' of
      Nothing -> return (TyVar tyvar)
      Just ty -> do
        ty' <- prune ty
        bindTyvar tyvar ty'
        return ty'
prune (TyFun tys ty) = do
    tys' <- mapM prune tys
    ty' <- prune ty
    return $ TyFun tys' ty'
prune (TyTuple tys) = TyTuple <$> mapM prune tys
prune (TyArr ty) = TyArr <$> prune ty
prune ty = return ty
