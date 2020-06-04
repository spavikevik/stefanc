{
{-# OPTIONS -w  #-}
module Lexer
  ( TokenWithMeta(..)
  , AlexPosn(..)
  , Token(..)
  , Alex(..)
  , runAlex
  ) where
import Prelude hiding (lex)
import Control.Monad (liftM)
}

%wrapper "monadUserState"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  $white+                               ;

  -- KEYWORDS
  auto                                  { tokenizeAs Auto }
  break                                 { tokenizeAs Break }
  case                                  { tokenizeAs Case }
  const                                 { tokenizeAs Const }
  continue                              { tokenizeAs Continue }
  default                               { tokenizeAs Default }
  do                                    { tokenizeAs Do }
  double                                { tokenizeAs Double }
  else                                  { tokenizeAs Else }
  enum                                  { tokenizeAs Enum }
  extern                                { tokenizeAs Extern }
  float                                 { tokenizeAs Float }
  for                                   { tokenizeAs For }
  goto                                  { tokenizeAs Goto }
  if                                    { tokenizeAs If }
  int                                   { tokenizeAs Int }
  long                                  { tokenizeAs Long }
  register                              { tokenizeAs Register }
  return                                { tokenizeAs Return }
  short                                 { tokenizeAs Short }
  signed                                { tokenizeAs Signed }
  sizeof                                { tokenizeAs Sizeof }
  static                                { tokenizeAs Static }
  struct                                { tokenizeAs Struct }
  switch                                { tokenizeAs Switch }
  typedef                               { tokenizeAs Typedef }
  union                                 { tokenizeAs Union }
  unsigned                              { tokenizeAs Unsigned }
  void                                  { tokenizeAs Void }
  volatile                              { tokenizeAs Volatile }
  while                                 { tokenizeAs While }

  -- IDENTIFIER
  \_? $alpha [$alpha $digit \_]*        { tokenizeAs' $ Identifier }

  -- LITERALS
  $digit+                               { tokenizeAs' $ IntLit . read }
  $digit+ \. $digit+                    { tokenizeAs' $ FloatLit . read }
  \" .* \"                              { tokenizeAs' $ String }

  -- OPERATORS
  "="                                   { tokenizeAs Assign }
  "+"                                   { tokenizeAs Plus }
  "-"                                   { tokenizeAs Minus }
  "*"                                   { tokenizeAs Asterisk }
  "/"                                   { tokenizeAs Slash }
  "<"                                   { tokenizeAs LessThan }
  "<="                                  { tokenizeAs LessThanEq }
  ">"                                   { tokenizeAs GreaterThan }
  ">="                                  { tokenizeAs GreaterThanEq }
  "=="                                  { tokenizeAs Equal }
  "!="                                  { tokenizeAs NotEqual }
  "!"                                   { tokenizeAs Bang }

   -- DELIMITERS
  ","                                   { tokenizeAs Comma }
  ";"                                   { tokenizeAs Semicolon }
  ":"                                   { tokenizeAs Colon }
  "{"                                   { tokenizeAs LBrace }
  "}"                                   { tokenizeAs RBrace }
  "("                                   { tokenizeAs LParen }
  ")"                                   { tokenizeAs RParen }
  "["                                   { tokenizeAs LBracket }
  "]"                                   { tokenizeAs RBracket }                      

{
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<none>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

data TokenWithMeta = TokenWithMeta AlexPosn Token

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
  RBracket          |
  EOF
  deriving (Eq, Show)

alexEOF :: Alex TokenWithMeta
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  return $ TokenWithMeta pos EOF

tokenizeAs' :: (String -> Token) -> AlexAction TokenWithMeta
tokenizeAs' tokenizer = \(pos, _, _, input) len -> return $ TokenWithMeta pos (tokenizer (take len input))

tokenizeAs :: Token -> AlexAction TokenWithMeta
tokenizeAs = tokenizeAs' . const
}
  