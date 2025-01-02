module Parser where

import AST
import Lexer

parser :: [Token] -> Either Terms String 
parser tokens =
    case sr [] tokens of
        [PT term] -> Left term
        [Err msg] -> Right $ "Parse error: " ++ msg
        stack -> Right $ "Incomplete parse. Remaining stack: " ++ show stack