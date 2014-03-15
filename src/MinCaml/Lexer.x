{
module MinCaml.Lexer where

import MinCaml.Types

import Prelude hiding (lex)

}

%wrapper "monadUserState"


$space = [ \ \t \r ]                     -- horizontal white space

$letter      = [a-zA-Z_]                 -- first letter of variables
$identletter = [a-zA-Z_0-9]              -- letters for rest of variables

$digit    = 0-9                          -- decimal digits

@digits   = $digit+
@mantpart  = @digits \. @digits
@mantpart' = @digits \.

tokens :-

  <0> $white+               ;
  <0> "(*"                  { begin state_comment }
  <state_comment> "*)"      { begin 0 }
  <state_comment> .         ;
  <state_comment> $white+   ;
  <0> $letter $identletter* { ident }
  <0> "("                   { tok LParen }
  <0> ")"                   { tok RParen }
  <0> @digits               { tokWValue Int }
  <0> @mantpart             { tokWValue Float }
  <0> @mantpart'            { convertHaskellFloat }
  <0> "-"                   { tok Minus }
  <0> "+"                   { tok Plus }
  <0> "-."                  { tok MinusDot }
  <0> "+."                  { tok PlusDot }
  <0> "*."                  { tok ASTDot }
  <0> "/."                  { tok SlashDot }
  <0> "="                   { tok Equal }
  <0> "<>"                  { tok LessGreater }
  <0> "<="                  { tok LessEqual }
  <0> ">="                  { tok GreaterEqual }
  <0> "<"                   { tok Less }
  <0> ">"                   { tok Greater }
  <0> ","                   { tok Comma }

  <0> "Array.create"        { tok ArrayCreate }
  <0> "."                   { tok Dot }
  <0> "<-"                  { tok LessMinus }
  <0> ";"                   { tok Semicolon }
  <0> "()"                  { tok LParenRParen }

  \EOF                      { tok EOF }

{

type AlexUserState = Int -- number of last fresh var

alexInitUserState = 0


alexEOF :: Alex TokPos
alexEOF = return (EOF, AlexPn (-1) (-1) (-1))


data Token
    = Bool Bool | Int Int | Float Float | Not | Minus
    | Plus | MinusDot | PlusDot | ASTDot | SlashDot | Equal
    | LessGreater | LessEqual | GreaterEqual | Less | Greater
    | If | Then | Else | Ident Id | Let | In | Rec | Comma
    | ArrayCreate | Dot | LessMinus | Semicolon | LParen | RParen
    | LParenRParen | EOF
    deriving (Show, Eq)

type TokPos = (Token, AlexPosn)


tokWValue :: Read a => (a -> Token) -> AlexInput -> Int -> Alex TokPos
tokWValue tok (posn,_,_,s) len = return (tok (read $ take len s), posn)


convertHaskellFloat :: AlexInput -> Int -> Alex TokPos
convertHaskellFloat (posn,_,_,s) len = return (Float (read $ take len s ++ "0"), posn)


tok :: Token -> AlexInput -> Int -> Alex TokPos
tok t (posn,_,_,_) _ = return (t, posn)


ident :: AlexAction TokPos
ident (posn,_,_,s) len = Alex $ \s@AlexState{alex_ust=var} ->
    let (var', ret) = tok var
    in Right (s{alex_ust=var'}, (ret, posn))
  where
    tok :: Int -> (Int, Token)
    tok var = case (take len s) of
                "true"         -> (var, Bool True)
                "false"        -> (var, Bool False)
                "not"          -> (var, Not)
                "if"           -> (var, If)
                "then"         -> (var, Then)
                "else"         -> (var, Else)
                "let"          -> (var, Let)
                "in"           -> (var, In)
                "rec"          -> (var, Rec)
                "Array.create" -> (var, ArrayCreate)
                -- I don't want to share any state between lexer and parser, so
                -- var_l$ prefix is used for fresh variables generated in lexing stage
                -- and var_p$ prefix is used for ones generated in parsing stage.
                "_"            -> (var+1, Ident $ "var_l$" ++ show var)
                ident'         -> (var, Ident ident')


lex :: String -> Either String [TokPos]
lex str = runAlex str loop
  where loop = do
          t@(tok, _) <- alexMonadScan
          if tok == EOF
            then return [t]
            else do toks <- loop
                    return (t : toks)


lexFile :: FilePath -> IO (Either String [TokPos])
lexFile p = fmap lex (readFile p)


}
