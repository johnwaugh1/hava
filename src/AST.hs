module AST where

-- Terms Definitions
data Terms
        = Var String
        | Num Integer
        | Float Double
        | BoolLit Bool
        | StrLit String
        | BinOp String Terms Terms
        | UnOp String Terms
        | If Terms Terms Terms
        | While Terms Terms
        | For Terms Terms Terms
        | Block [Terms]
        | MethodDecl String [String] Terms
        | MethodCall String [Terms]
        | ClassDecl String [Terms]
        | Return Terms
        | Assign String Terms
        | Expr Terms
        | ErrTerm String
    deriving (Eq, Show)  
    
-- Token Definitions
data Token = Keyword String
        | Identifier String
        | IntegerLiteral Integer
        | FloatingPointLiteral Double
        | StringLiteral String
        | BoolLiteral Bool
        | Operator String
        | Symbol Char
        | LPar | RPar | LBrace | RBrace | LBracket | RBracket
        | SemiColon | Comma | Colon | QuestionMark | Dot 
        | Equals | Plus | Minus | Star | Slash | Modulo
        | And | Or | Not | GreaterThan | LessThan 
        | GreaterThanEqual | LessThanEqual | EqualEqual | NotEqual
        | SingleLineComment String | MultiLineComment String
        | PT Terms
        | Err String
    deriving (Eq, Show)