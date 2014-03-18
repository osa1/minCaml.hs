{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module MinCaml.Types where


import           Data.Generics


-------------------------------------------------------------------------------
-- Terms

data Tm
    = TUnit
    | TBool Bool
    | TInt Int
    | TFloat Float
    | TNot Tm
    | TNeg Tm
    | TAdd Tm Tm
    | TSub Tm Tm
    | TFNeg Tm
    | TFAdd Tm Tm
    | TFSub Tm Tm
    | TFMul Tm Tm
    | TFDiv Tm Tm
    | TEq Tm Tm
    | TLE Tm Tm
    | TIf Tm Tm Tm
    | TLet (Id, Ty) Tm Tm
    | TVar Id
    | TLetRec FunDef Tm
    | TApp Tm [Tm]
    | TTuple [Tm]
    | TLetTuple [(Id, Ty)] Tm Tm -- ^ let (x1, ..., xn) = e1 in e2
    | TArr Tm Tm -- ^ Array.create e1 e2
    | TGet Tm Tm -- ^ e1.(e2)
    | TPut Tm Tm Tm -- ^ e1.(e2) <- e3
    deriving (Show)

deriving instance Data Tm
deriving instance Typeable Tm


data FunDef = FunDef (Id, Ty) [(Id, Ty)] Tm deriving (Show)

deriving instance Data FunDef
deriving instance Typeable FunDef

-------------------------------------------------------------------------------
-- Types

data Ty
    = TyUnit
    | TyBool
    | TyInt
    | TyFloat
    | TyFun [Ty] Ty
    | TyTuple [Ty]
    | TyArr Ty
    | TyVar TyVar
    deriving (Show, Eq, Ord)

deriving instance Data Ty
deriving instance Typeable Ty


type TyVar = Int


-------------------------------------------------------------------------------
-- Identifiers

type Id = String
