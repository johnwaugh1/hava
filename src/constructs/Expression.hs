module Expression (parseExpression) where

import Lexer
import Parser

parseExpression :: [Token] -> [Token] -> [Token]
parseExpression (PT (Num n) : Plus : PT (Num m) : s) q =
    sr (PT (Num (n + m)) : s) q