module ControlFlow (parseControlFlow) where

import Lexer
import Parser

parseIfElse :: [Token] -> [Token] -> [Token]
parseIfElse (Keyword "if" : LPar : PT condition : RPar : LBrace : body : RBrace : Keyword "else" : LBrace : elseBody : RBrace : s) q =
    sr (If condition body elseBody : s) q

parseWhileLoop :: [Token] -> [Token] -> [Token]
parseWhileLoop (Keyword "while" : LPar : PT Condition : RPar : LBrace : body : RBrace : s) q =
    sr (While condition body : s) q