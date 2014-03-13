{-# OPTIONS_GHC -Wall #-}
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
    { unifications :: M.Map TyVar Ty
    , freshTyVar   :: TyVar
    } deriving (Show)


data UnificationError
    = OccursCheck Ty Ty
    | UnificationError Ty Ty
    | StrErr String
    deriving (Show)

instance Error UnificationError where
    strMsg = StrErr


newtype Unify a = Unify { unwrapUnify :: StateT TypingState (ErrorT UnificationError Identity) a }
    deriving (Functor, Applicative, Monad, MonadState TypingState, MonadError UnificationError)


lookupTyvar :: TyVar -> Unify (Maybe Ty)
lookupTyvar tyvar = do
    us <- gets unifications
    return $ M.lookup tyvar us


bindTyvar :: TyVar -> Ty -> Unify ()
bindTyvar var ty =
    modify (\s@TypingState{unifications=us} -> s{unifications=M.insert var ty us})


-------------------------------------------------------------------------------
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
      (TyVar var1, TyVar var2) -> undefined -- TODO
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
