{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  $white+                               ;

  -- KEYWORDS
  for                                   { \s -> For }
  while                                 { \s -> While }
  "if"                                  { \s -> If }
  "else"                                { \s -> Else }
  static                                { \s -> Static }
  "do"                                  { \s -> Do }
  break                                 { \s -> Break }
  continue                              { \s -> Continue }
  extern                                { \s -> Extern }
  return                                { \s -> Return }

  -- IDENTIFIER
  \_? $alpha [$alpha $digit \_]*        { \s -> Identifier s }

  -- LITERALS
  $digit+                               { \s -> Int (read s) }
  $digit+ \. $digit+                    { \s -> Double (read s) }
  \" .* \"                              { \s -> String s }

  -- OPERATORS
  "="                                   { \s -> Assign }
  "+"                                   { \s -> Plus }
  "-"                                   { \s -> Minus }
  "*"                                   { \s -> Asterisk }
  "/"                                   { \s -> Slash }
  "<"                                   { \s -> LessThan }
  "<="                                  { \s -> LessThanEq }
  ">"                                   { \s -> GreaterThan }
  ">="                                  { \s -> GreaterThanEq }
  "=="                                  { \s -> Equal }
  "!="                                  { \s -> NotEqual }
  "!"                                   { \s -> Bang }

   -- DELIMITERS
  ","                                   { \s -> Comma }
  ";"                                   { \s -> Semicolon }
  ":"                                   { \s -> Colon }
  "{"                                   { \s -> LBrace }
  "}"                                   { \s -> RBrace }
  "("                                   { \s -> LParen }
  ")"                                   { \s -> RParen }
  "["                                   { \s -> LBracket }
  "]"                                   { \s -> RBracket }                      

{
data Token          =
  For               |
  While             |
  If                |
  Else              |
  Static            |
  Do                |
  Break             |
  Continue          |
  Extern            |
  Return            |
  Identifier String |
  String String     |
  Int Int           |
  Double Double     |
  Sym Char          |
  Assign            |
  Plus              |
  Minus             |
  Asterisk          |
  Slash             |
  LessThan          |
  LessThanEq        |
  GreaterThan       |
  GreaterThanEq     |
  Equal             |
  NotEqual          |
  Bang              |
  Comma             |
  Semicolon         |
  Colon             |
  LBrace            |
  RBrace            |
  LParen            |
  RParen            |
  LBracket          |
  RBracket 
  deriving (Eq, Show)

main = do
  s <- getContents
  putStrLn $ show (alexScanTokens s)
}
  