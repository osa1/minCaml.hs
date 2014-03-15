
-- | Alpha conversion: give every binder a unique name
module MinCaml.AlphaConv (alphaConv) where


import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           MinCaml.KNormal
import           MinCaml.Types


alphaConv :: KNormal -> KNormal
alphaConv k = evalState (alphaConv' M.empty k) 0


freshId :: State Int Id
freshId = do
    i <- get
    put (i + 1)
    return $ "var_a$" ++ show i


alphaConv' :: M.Map Id Id -> KNormal -> State Int KNormal
alphaConv' _ KUnit = return KUnit
alphaConv' _ k@KInt{} = return k
alphaConv' _ k@KFloat{} = return k
alphaConv' env (KNeg x) = return $ KNeg $ find x env
alphaConv' env (KAdd x y) = return $ KAdd (find x env) (find y env)
alphaConv' env (KSub x y) = return $ KSub (find x env) (find y env)
alphaConv' env (KFNeg x) = return $ KFNeg $ find x env
alphaConv' env (KFAdd x y) = return $ KFAdd (find x env) (find y env)
alphaConv' env (KFSub x y) = return $ KFSub (find x env) (find y env)
alphaConv' env (KFMul x y) = return $ KFMul (find x env) (find y env)
alphaConv' env (KFDiv x y) = return $ KFDiv (find x env) (find y env)
alphaConv' env (KIfEq x y e1 e2) = KIfEq (find x env) (find y env) <$> alphaConv' env e1 <*> alphaConv' env e2
alphaConv' env (KIfLe x y e1 e2) = KIfLe (find x env) (find y env) <$> alphaConv' env e1 <*> alphaConv' env e2
alphaConv' env (KLet (x, t) e1 e2) = do
    x' <- freshId
    KLet (x', t) <$> alphaConv' env e1 <*> alphaConv' (M.insert x x' env) e2
alphaConv' env (KVar i) = return $ KVar $ find i env
alphaConv' env (KLetRec (KFunDef (x, t) args e1) e2) = do
    x' <- freshId
    let argIds = map fst args
    freshArgIds <- replicateM (length args) freshId
    let env' = M.fromList (zip argIds freshArgIds) `M.union` M.insert x x' env
    e1' <- alphaConv' env' e1
    e2' <- alphaConv' env e2
    return $ KLetRec (KFunDef (x', t) (zip freshArgIds (map snd args)) e1') e2'
alphaConv' env (KApp e es) = return $ KApp (find e env) (map (flip find env) es)
alphaConv' env (KTuple xs) = return $ KTuple $ map (flip find env) xs
alphaConv' env (KLetTuple bs t e) = do
    freshIds <- replicateM (length bs) freshId
    let env' = M.fromList (zip (map fst bs) freshIds) `M.union` env
    KLetTuple (zip freshIds (map snd bs)) (find t env) <$> alphaConv' env' e
alphaConv' env (KGet x y) = return $ KGet (find x env) (find y env)
alphaConv' env (KPut x y z) = return $ KPut (find x env) (find y env) (find z env)
alphaConv' _ e@KExtArray{} = return e
alphaConv' env (KExtFunApp x args) = return $ KExtFunApp x (map (flip find env) args)


find :: Id -> M.Map Id Id -> Id
find i env = fromMaybe i $ M.lookup i env
