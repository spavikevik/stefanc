{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  $white+                               ;

  -- KEYWORDS
  auto                                  { \s -> Auto }
  break                                 { \s -> Break }
  case                                  { \s -> Case }
  const                                 { \s -> Const }
  continue                              { \s -> Continue }
  default                               { \s -> Default }
  do                                    { \s -> Do }
  double                                { \s -> Double }
  else                                  { \s -> Else }
  enum                                  { \s -> Enum }
  extern                                { \s -> Extern }
  float                                 { \s -> Float }
  for                                   { \s -> For }
  goto                                  { \s -> Goto }
  if                                    { \s -> If }
  int                                   { \s -> Int }
  long                                  { \s -> Long }
  register                              { \s -> Register }
  return                                { \s -> Return }
  short                                 { \s -> Short }
  signed                                { \s -> Signed }
  sizeof                                { \s -> Sizeof }
  static                                { \s -> Static }
  struct                                { \s -> Struct }
  switch                                { \s -> Switch }
  typedef                               { \s -> Typedef }
  union                                 { \s -> Union }
  unsigned                              { \s -> Unsigned }
  void                                  { \s -> Void }
  volatile                              { \s -> Volatile }
  while                                 { \s -> While }

  -- IDENTIFIER
  \_? $alpha [$alpha $digit \_]*        { \s -> Identifier s }

  -- LITERALS
  $digit+                               { \s -> IntLit (read s) }
  $digit+ \. $digit+                    { \s -> FloatLit (read s) }
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
  Auto              |
  Break             |
  Case              |
  Char              |
  Const             |
  Continue          |
  Default           |
  Do                |
  Double            |
  Else              |
  Enum              |
  Extern            |
  Float             |
  For               |
  Goto              |
  If                |
  Int               |
  Long              |
  Register          |
  Return            |
  Short             |
  Signed            |
  Sizeof            |
  Static            |
  Struct            |
  Switch            |
  Typedef           |
  Union             |
  Unsigned          |
  Void              |
  Volatile          |
  While             |
  Identifier String |
  String String     |
  IntLit Int        |
  FloatLit Float    |
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
  