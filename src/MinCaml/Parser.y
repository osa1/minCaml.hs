{
{-# OPTINS_GHC -w #-}

module MinCaml.Parser where

import Prelude hiding (lex)
import Control.Monad.State

import MinCaml.Lexer
import MinCaml.Types

}

%name parse
%tokentype { TokPos }
%error { parseError }
%monad { State ParserState }

%right PREC_LET
%right ';'
%right PREC_IF
%right '<-'
%left ','
%left '=' '<>' '<' '>' '<=' '>='
%left '+' '-' '+.' '-.'
%left '*.' '/.'
%right PREC_UNARY_MINUS
%left PREC_APP
%left '.'

%token
  bool    { (Bool $$, _) }
  int     { (Int $$, _) }
  float   { (Float $$, _) }
  not     { (Not, _) }
  '-'     { (Minus, _) }
  '+'     { (Plus, _) }
  '-.'    { (MinusDot, _) }
  '+.'    { (PlusDot, _) }
  '*.'    { (ASTDot, _) }
  '/.'    { (SlashDot, _) }
  '='     { (Equal, _) }
  '<>'    { (LessGreater, _) }
  '<='    { (LessEqual, _) }
  '>='    { (GreaterEqual, _) }
  '<'     { (Less, _) }
  '>'     { (Greater, _) }
  if      { (If, _) }
  then    { (Then, _) }
  else    { (Else, _) }
  ident   { (Ident $$, _) }
  let     { (Let, _) }
  in      { (In, _) }
  rec     { (Rec, _) }
  ','     { (Comma, _) }
  Arraycreate { (ArrayCreate, _) }
  '.'     { (Dot, _) }
  '<-'    { (LessMinus, _) }
  ';'     { (Semicolon, _) }
  '('     { (LParen, _) }
  ')'     { (RParen, _) }
  '()'    { (LParenRParen, _) }

%%

Exp : SimpleExp                  { $1 }
    | not Exp
      %prec PREC_APP             { TNot $2 }
    | '-' Exp
      %prec PREC_UNARY_MINUS     { case $2 of
                                     { TFloat f -> TFNeg (TFloat f)
                                     ; e -> TNeg e } }
    | Exp '+' Exp                { TAdd $1 $3 }
    | Exp '-' Exp                { TSub $1 $3 }
    | Exp '=' Exp                { TEq $1 $3 }
    | Exp '<>' Exp               { TNot (TEq $1 $3) }
    | Exp '<' Exp                { TNot (TLE $3 $1) }
    | Exp '>' Exp                { TNot (TLE $1 $3) }
    | Exp '<=' Exp               { TLE $1 $3 }
    | Exp '>=' Exp               { TLE $3 $1 }
    | if Exp then Exp else Exp
      %prec PREC_IF              { TIf $2 $4 $6 }
    | '-.' Exp
      %prec PREC_UNARY_MINUS     { TFNeg $2 }
    | Exp '+.' Exp               { TFAdd $1 $3 }
    | Exp '-.' Exp               { TFSub $1 $3 }
    | Exp '*.' Exp               { TFMul $1 $3 }
    | Exp '/.' Exp               { TFDiv $1 $3 }
    | let ident '=' Exp in Exp
      %prec PREC_LET             {% freshTy >>= \ty -> return (TLet ($2, ty) $4 $6) }
    | let rec Fundef in Exp
      %prec PREC_LET             { TLetRec $3 $5 }
    | Exp ActualArgs
      %prec PREC_APP             { TApp $1 $2 }
    | Elems                      { TTuple $1 }
    | let '(' Pat ')' '=' Exp in Exp
                                 { TLetTuple $3 $6 $8 }
    | SimpleExp '.' '(' Exp ')' '<-' Exp
                                 { TPut $1 $4 $7 }
    | Exp ';' Exp                {% freshVar >>= \var -> return (TLet (var, TyUnit) $1 $3) }
    | Arraycreate SimpleExp SimpleExp
      %prec PREC_APP             { TArr $2 $3 }

SimpleExp
    : '(' Exp ')'                { $2 }
    | '()'                       { TUnit }
    | bool                       { TBool $1 }
    | int                        { TInt $1 }
    | float                      { TFloat $1 }
    | ident                      { TVar $1 }
    | SimpleExp '.' '(' Exp ')'  { TGet $1 $4 }

Fundef
    : ident FormalArgs '=' Exp   {% freshTy >>= \ty -> return (FunDef ($1, ty) $2 $4) }

FormalArgs
    : ident FormalArgs           {% freshTy >>= \ty -> return (($1, ty) : $2) }
    | ident                      {% freshTy >>= \ty -> return [($1, ty)] }

ActualArgs
    : ActualArgs SimpleExp
      %prec PREC_APP             { $1 ++ [$2] }
    | SimpleExp
      %prec PREC_APP             { [$1] }

Elems
    : Elems ',' Exp              { $1 ++ [$3] }
    | Exp ',' Exp                { [$1, $3] }

Pat : Pat ',' ident              {% freshTy >>= \ty -> return ($1 ++ [($3, ty)]) }
    | ident ',' ident            {% freshTy >>= \ty1 -> freshTy >>= \ty2 -> return [($1, ty1), ($3, ty2)] }

{

data ParserState = ParserState
    { freshTy_ :: TyVar
    , freshId_ :: Int
    }

initParserState = ParserState 0 0

freshTy :: State ParserState Ty
freshTy = do
    s@ParserState{freshTy_=freshTy} <- get
    put s{freshTy_ = freshTy + 1}
    return $ TyVar freshTy

freshVar :: State ParserState Id
freshVar = do
    s@ParserState{freshId_=freshId} <- get
    put s{freshId_ = freshId + 1}
    return $ "var_p" ++ show freshId

parseError :: [TokPos] -> a
parseError ((_, posn) : ts) = error $ "Parse error at " ++ show posn

parseStr :: String -> Either String Tm
parseStr str =
    case lex str of
      Left err -> Left err
      Right tokens -> Right $ evalState (parse $ init tokens) initParserState

parseStr' :: String -> Either String (Tm, TyVar)
parseStr' str =
    case lex str of
      Left err -> Left err
      Right tokens ->
        let (tm, state) = runState (parse $ init tokens) initParserState
        in Right (tm, freshTy_ state)

parseFile :: FilePath -> IO (Either String Tm)
parseFile path = readFile path >>= return . parseStr

parseFile' :: FilePath -> IO (Either String (Tm, TyVar))
parseFile' path = readFile path >>= return . parseStr'

}
