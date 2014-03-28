

-- | Closure conversion
module MinCaml.ClosureConv
  ( closureConv
  , CC (..)
  , Closure (..)
  , FunDef (..)
  , pprint
  , pprintDecls
  ) where


-------------------------------------------------------------------------------
import           Control.Applicative       hiding (empty)
import           Control.Monad.State
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import qualified Data.Set                  as S
import           Text.PrettyPrint.HughesPJ


import           MinCaml.KNormal           hiding (fvs, pprint)
import           MinCaml.Types             hiding (FunDef)
import           MinCaml.Typing            (init_env)

import           Debug.Trace
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data CC
    = CUnit
    | CInt Int
    | CFloat Float
    | CNeg Id
    | CAdd Id Id
    | CSub Id Id
    | CFNeg Id
    | CFAdd Id Id
    | CFSub Id Id
    | CFMul Id Id
    | CFDiv Id Id
    | CIfEq Id Id CC CC
    | CIfLe Id Id CC CC
    | CLet (Id, Ty) CC CC
    | CVar Id
    | CMkCls (Id, Ty) Closure CC
    | CAppCls Id Ty [Id]
    | CAppDir Id [Id]
    | CTuple [(Id, Ty)]
    | CLetTuple [(Id, Ty)] Id CC
    | CGet Id Id
    | CPut Id Id Id
    deriving (Show)

data Closure = Closure
    { entry :: Id
    , cFvs  :: M.Map Id Ty
    } deriving (Show)

data FunDef = FunDef
    { name    :: Id
    , ty      :: Ty
    , fargs   :: [(Id, Ty)]
    , fdFvs   :: M.Map Id Ty
    , closure :: Bool
    , body    :: Maybe CC
    } deriving (Show)


fvs :: CC -> S.Set Id
fvs CUnit = S.empty
fvs (CInt _) = S.empty
fvs (CFloat _) = S.empty
fvs (CNeg i) = S.singleton i
fvs (CAdd i1 i2) = S.fromList [i1, i2]
fvs (CSub i1 i2) = S.fromList [i1, i2]
fvs (CFNeg i) = S.singleton i
fvs (CFAdd i1 i2) = S.fromList [i1, i2]
fvs (CFSub i1 i2) = S.fromList [i1, i2]
fvs (CFMul i1 i2) = S.fromList [i1, i2]
fvs (CFDiv i1 i2) = S.fromList [i1, i2]
fvs (CIfEq i1 i2 c1 c2) = S.insert i1 $ S.insert i2 $ fvs c1 `S.union` fvs c2
fvs (CIfLe i1 i2 c1 c2) = S.insert i1 $ S.insert i2 $ fvs c1 `S.union` fvs c2
fvs (CLet (i, _) c1 c2) = fvs c1 `S.union` S.delete i (fvs c2)
fvs (CVar i) = S.singleton i
fvs (CMkCls (i, _) (Closure _ freevars) e) = S.delete i (M.keysSet freevars `S.union` fvs e)
fvs (CAppCls c _ ids) = S.fromList $ c : ids
fvs (CAppDir _ ids) =
    -- here we deliberately ignore function name while generating free
    -- variables, see comments in `cc`.
    S.fromList ids
fvs (CTuple is) = S.fromList $ map fst is
fvs (CLetTuple is i c) = S.insert i $ fvs c `S.difference` S.fromList (map fst is)
fvs (CGet i1 i2) = S.fromList [i1, i2]
fvs (CPut i1 i2 i3) = S.fromList [i1, i2, i3]


closureConv :: KNormal -> (CC, M.Map Id FunDef)
closureConv k = runState (cc init_env (S.singleton "print_int") k) M.empty


cc :: M.Map Id Ty -> S.Set Id -> KNormal -> State (M.Map Id FunDef) CC
cc env known k =
    case k of
      KUnit -> return CUnit
      KInt i -> return $ CInt i
      KFloat f -> return $ CFloat f
      KNeg x -> return $ CNeg x
      KAdd x y -> return $ CAdd x y
      KSub x y -> return $ CSub x y
      KFNeg x -> return $ CFNeg x
      KFAdd x y -> return $ CFAdd x y
      KFSub x y -> return $ CFSub x y
      KFMul x y -> return $ CFMul x y
      KFDiv x y -> return $ CFDiv x y
      KIfEq x y e1 e2 -> CIfEq x y <$> cc env known e1 <*> cc env known e2
      KIfLe x y e1 e2 -> CIfLe x y <$> cc env known e1 <*> cc env known e2
      KLet (x, t) e1 e2 -> CLet (x, t) <$> cc env known e1 <*> cc (M.insert x t env) known e2
      KVar x -> return $ CVar x

      KLetRec (KFunDef (x, t) args e1) e2 -> do
        -- * if e1 has free variables, closure convert it, add it to
        --   known set, and return MkCls
        -- * if x appears in e2 as a variable(not a label), closure convert e1
        --   and return MkCls. From the tutorial:
        --   "This is because a user who receives x as a value does not know
        --   in general if it has a free variable or not, and therefore must
        --   anyway use AppCls (not AppDir) to call the function through its closure."
        -- * otherwise function is closed and is not returned by some other
        --   expression, just closure convert e1 and return e2

        -- step 1: assume that e1 is closed and thus x is known closed function:
        toplevel_backup <- get
        let env' = M.fromList args `M.union` M.insert x t env
            known' = S.insert x known
        e1' <- cc env' known' e1
        let e1'fvs = fvs e1' `S.difference` S.fromList (map fst args)
        -- now in order our assumption to hold, e1'fvs should be empty
        (known_, e1_, cls) <-
          if S.null e1'fvs then
            return (known', e1', False)
          else do
            -- step 2: assumption did not hold, closure convert
            -- e1' without having x as known closed function
            put toplevel_backup
            e1_ <- cc env' known e1
            return (known, e1_, True)

        let e1'fvs' = S.delete x e1'fvs
            zts = M.fromList $ map (\z -> (z, fromJust $ M.lookup z env')) (S.toList e1'fvs')
        -- add function part of the closure to top level declarations
        let declName = if cls then x ++ "_fun" else x
        modify (M.insert x (FunDef declName t args zts cls (Just e1_)))

        e2' <- cc env' known_ e2
        if S.member x (fvs e2') then
          -- step 3: x is used in e2' as a variable(i.e. passed to some
          -- other function, returned etc.), return MkCls
          let addTys :: S.Set Id -> M.Map Id Ty
              addTys = M.fromList . map (\i -> (i, fromJust $ M.lookup i env)) . S.toList
          in return $ CMkCls (x, t) (Closure x (addTys e1'fvs')) e2'
        else
          -- x is used in e2' as function part of application, we can use
          -- AppDir in this case, no need for closure conversion of e2'
          return e2'

      KApp x fty args
        | S.member x known -> return $ CAppDir x args
        | otherwise -> return $ CAppCls x fty args
      KTuple xs -> return $ CTuple xs
      KLetTuple xs b e -> CLetTuple xs b <$> cc (M.fromList xs `M.union` env) known e
      KGet x y -> return $ CGet x y
      KPut x y z -> return $ CPut x y z
      KExtFunApp x args -> return $ CAppDir ("min_caml_" ++ x) args


-------------------------------------------------------------------------------
-- | Pretty printer
pprint :: CC -> Doc
pprint CUnit = text "()"
pprint (CInt i) = int i
pprint (CFloat f) = float f
pprint (CNeg x) = parens $ char '-' <> text x
pprint (CAdd i1 i2) = parens $ text i1 <+> char '+' <+> text i2
pprint (CSub i1 i2) = parens $ text i1 <+> char '-' <+> text i2
pprint (CFNeg x) = parens $ text "-." <> text x
pprint (CFAdd i1 i2) = parens $ text i1 <+> text "+." <+> text i2
pprint (CFSub i1 i2) = parens $ text i1 <+> text "-." <+> text i2
pprint (CFMul i1 i2) = parens $ text i1 <+> text "*." <+> text i2
pprint (CFDiv i1 i2) = parens $ text i1 <+> text "/." <+> text i2
pprint (CIfEq i1 i2 c1 c2) = text "if" <+> text i1 <+> char '=' <+> text i2 <+> text "then"
                               $$ nest 4 (pprint c1)
                               $$ text "else"
                               $$ nest 4 (pprint c2)
pprint (CIfLe i1 i2 c1 c2) = text "if" <+> text i1 <+> char '<' <+> text i2 <+> text "then"
                               $$ nest 4 (pprint c1)
                               $$ text "else"
                               $$ nest 4 (pprint c2)
pprint (CLet (x, t) c1 c2) = sep [ hang (text "let" <+> text x <> char ':' <+> text (show t) <+> char '=')
                                        4 (pprint c1)
                                 , text "in"
                                 ] $$ pprint c2
pprint (CVar x) = text x
pprint (CMkCls _ (Closure entry fvs) c) =
    text "closure"
    <> parens (sep $ punctuate (char ',') [text entry, text $ show fvs])
    $+$ pprint c
pprint (CAppCls x _ args) = text "c#" <> text x <+> hsep (map text args)
pprint (CAppDir x args) = text x <+> hsep (map text args)
pprint (CTuple idtys) = parens $ hcat $
    punctuate (text ", ") (map (\(i, t) -> text i <> char ':' <+> text (show t)) idtys)
pprint (CLetTuple xts i c) =
    sep [ text "let" <+> parens (pprintArgs xts) <+> char '=' <+> text i
        , text "in"
        ] $$ pprint c
pprint (CGet i1 i2) = parens (text i1) <> char '.' <> text i2
pprint (CPut i1 i2 i3) = parens (text i1) <> char '.' <> text i2 <+> text "<-" <+> text i3


pprintArgs :: [(Id, Ty)] -> Doc
pprintArgs = sep . punctuate (char ',') . map (\(x, t) -> text x <> text ": " <> text (show t))


-- | Pretty-print top-level declarations
pprintDecls :: M.Map Id FunDef -> Doc
pprintDecls defs = iter (M.toList defs)
  where
    iter [] = empty
    iter ((x, def) : rest) =
      hang (text x <> char ':' <+> text (show $ ty def) <+> char '=')
           4 (pprintFunDef def) $$ iter rest


pprintFunDef :: FunDef -> Doc
pprintFunDef (FunDef _ _ fargs fdFvs closure body) =
    hang (text "fn" <> parens (pprintArgs fargs)
                    <> brackets (pprintArgs $ M.toList fdFvs)
                    <+> if closure then text "<closure>" else empty
                    <+> char '=')
         2 (case body of
              Nothing -> text "<builtin>"
              Just body' -> pprint body')
