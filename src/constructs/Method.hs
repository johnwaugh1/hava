module Method (parseMethodDecl) where

import Lexer
import Parser

parseMethodDecl :: [Token] -> [Token] -> [Token]
parseMethodDecl (Keyword "public" : Keyword returnType : Identifier methodName : LPar : params : RPar : LBrace : body : RBrace : s) q =
    sr (MethodDecl returnType methodName params body : s) q
