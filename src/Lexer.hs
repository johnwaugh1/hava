import Data.Char
import Data.List

-- Token Definitions
data Token = Keyword String
           | Identifier String
           | IntegerLiteral Integer
           | FloatingPointLiteral Double
           | StringLiteral String
           | BoolLiteral Bool
           | Operator String
           | Symbol Char
           | KeywordIf | KeywordElse | KeywordWhile
           | KeywordReturn | KeywordInt | KeywordString
           | KeywordPublic | KeywordPrivate | KeywordClass
           | KeywordVoid | KeywordStatic | KeywordNew
           | LPar | RPar | LBrace | RBrace | SemiColon | Comma
           | Dot | Equals | Plus | Minus | Star | Slash
           | And | Or | Not | GreaterThan | LessThan | GreaterThanEqual | LessThanEqual
           | EqualEqual | NotEqual
           | SingleLineComment String | MultiLineComment String
           | Err String
    deriving (Eq, Show)

-- Lexer Implementation
lexer :: String -> [Token]
lexer "" = []
-- Symbols and Punctuation
lexer ('\\':xs) = Err "Invalid escape character" : lexer xs
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer ('{':xs) = LBrace : lexer xs
lexer ('}':xs) = RBrace : lexer xs
lexer (';':xs) = SemiColon : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer ('.':xs) = Dot : lexer xs
lexer ('=':xs) = Equals : lexer xs
lexer ('+':xs) = Plus : lexer xs
lexer ('-':xs) = Minus : lexer xs
lexer ('*':xs) = Star : lexer xs
lexer ('/':xs) = Slash : lexer xs
-- Operators
lexer ('&':'&':xs) = And : lexer xs
lexer ('|':'|':xs) = Or : lexer xs
lexer ('!':'=':xs) = NotEqual : lexer xs
lexer ('=':'=':xs) = EqualEqual : lexer xs
lexer ('>':'=':xs) = GreaterThanEqual : lexer xs
lexer ('<':'=':xs) = LessThanEqual : lexer xs
lexer ('>':xs) = GreaterThan : lexer xs
lexer ('<':xs) = LessThan : lexer xs
lexer ('!':xs) = Not : lexer xs
-- Whitespace
lexer (x:xs) | isSpace x = lexer xs
-- Keywords and Identifiers
lexer (x:xs) | isAlpha x = let (ident, rest) = span isAlphaNum xs
                           in case ident of
                                "if"    -> KeywordIf : lexer rest
                                "else"  -> KeywordElse : lexer rest
                                "while" -> KeywordWhile : lexer rest
                                "return" -> KeywordReturn : lexer rest
                                "int"   -> KeywordInt : lexer rest
                                "string" -> KeywordString : lexer rest
                                "public" -> KeywordPublic : lexer rest
                                "private" -> KeywordPrivate : lexer rest
                                "class" -> KeywordClass : lexer rest
                                "void"  -> KeywordVoid : lexer rest
                                "static" -> KeywordStatic : lexer rest
                                "new"   -> KeywordNew : lexer rest
                                _       -> Identifier (x:ident) : lexer rest
-- Integers
lexer (x:xs) | isDigit x = let (numStr, rest) = span isDigit xs
                           in IntegerLiteral (read (x:numStr)) : lexer rest
-- Floating-Point Numbers
lexer (x:xs) | isDigit x || x == '.' = let (numStr, rest) = span (\c -> isDigit c || c == '.') xs
                                      in FloatingPointLiteral (read (x:numStr)) : lexer rest
-- String Literals
lexer ('"':xs) = let (str, rest) = span (/= '"') xs
                 in StringLiteral str : lexer (tail rest)
-- Comments
lexer ('/':'/':xs) = let (comment, rest) = span (/= '\n') xs
                     in SingleLineComment comment : lexer rest
lexer ('/':'*':xs) = let (comment, rest) = span (/= '*') xs
                     in case rest of
                        ('*':'/':ys) -> MultiLineComment comment : lexer ys
                        _            -> Err "Unterminated comment" : lexer rest
-- Boolean Literals
lexer (x:xs) | x == 't' = BoolLiteral True : lexer xs
lexer (x:xs) | x == 'f' = BoolLiteral False : lexer xs
-- Invalid Characters
lexer xs = [Err (take 10 xs)]
