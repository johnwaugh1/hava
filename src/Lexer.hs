module Lexer where

import AST
import Data.Char
import Data.List

javaKeywords :: [String]
javaKeywords =
    [ "abstract", "continue", "for", "new", "switch"
    , "assert", "default", "goto", "package", "synchronized"
    , "boolean", "do", "if", "private", "this"
    , "break", "double", "implements", "protected", "throw"
    , "byte", "else", "import", "public", "throws"
    , "case", "enum", "instanceof", "return", "transient"
    , "catch", "extends", "int", "short", "try"
    , "char", "final", "interface", "static", "void"
    , "class", "finally", "long", "strictfp", "volatile"
    , "const", "float", "native", "super", "while"
    , "System", "out", "println", "String"
    ]

-- Lexer Implementation
lexer :: String -> [Token]
lexer "" = []
-- Whitespace
lexer (x:xs) | isSpace x = lexer xs
-- Symbols
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer ('{':xs) = LBrace : lexer xs
lexer ('}':xs) = RBrace : lexer xs
lexer ('[':xs) = LBracket : lexer xs
lexer (']':xs) = RBracket : lexer xs
lexer (';':xs) = SemiColon : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer ('.':xs) = Dot : lexer xs
lexer (':':xs) = Colon : lexer xs
lexer ('?':xs) = QuestionMark : lexer xs
-- Operators
lexer ('=':'=':xs) = EqualEqual : lexer xs
lexer ('!':'=':xs) = NotEqual : lexer xs
lexer ('>':'=':xs) = GreaterThanEqual : lexer xs
lexer ('<':'=':xs) = LessThanEqual : lexer xs
lexer ('&':'&':xs) = And : lexer xs
lexer ('|':'|':xs) = Or : lexer xs
lexer ('=':xs) = Equals : lexer xs
lexer ('+':xs) = Plus : lexer xs
lexer ('-':xs) = Minus : lexer xs
lexer ('*':xs) = Star : lexer xs
lexer ('/':xs) = Slash : lexer xs
lexer ('%':xs) = Modulo : lexer xs
lexer ('>':xs) = GreaterThan : lexer xs
lexer ('<':xs) = LessThan : lexer xs
lexer ('!':xs) = Not : lexer xs

-- Single-line Comments
lexer ('/':'/':xs) = 
    let (comment, rest) = span (/= '\n') xs
    in SingleLineComment comment : lexer rest

-- Multi-line Comments
lexer ('/':'*':xs) = parseMultiLineComment "" xs
  where
    parseMultiLineComment :: String -> String -> [Token]
    parseMultiLineComment acc ('*':'/':ys) = MultiLineComment acc : lexer ys
    parseMultiLineComment acc (y:ys) = parseMultiLineComment (acc ++ [y]) ys
    parseMultiLineComment acc [] = [Err "Unterminated comment"]

-- String Literals
lexer ('"':xs) = parseStringLiteral xs
  where
    parseStringLiteral :: String -> [Token]
    parseStringLiteral input = parse "" input
      where
        parse acc ('\\':'"':ys) = parse (acc ++ "\"") ys
        parse acc ('\\':'\\':ys) = parse (acc ++ "\\") ys
        parse acc ('"':ys) = StringLiteral acc : lexer ys
        parse acc [] = [Err "Unterminated string literal"]
        parse acc (y:ys) = parse (acc ++ [y]) ys

-- Boolean Literals
lexer ('t':'r':'u':'e':xs) = BoolLiteral True : lexer xs
lexer ('f':'a':'l':'s':'e':xs) = BoolLiteral False : lexer xs

-- Floating-Point & Integer Literals
lexer (x:xs) 
    | isDigit x || x == '.' = 
        let (numStr, rest) = span (\c -> isDigit c || c == '.') (x:xs)
        in if '.' `elem` numStr
            then FloatingPointLiteral (read numStr) : lexer rest
            else IntegerLiteral (read numStr) : lexer rest

-- Keywords and Identifiers
lexer (x:xs) 
    | isAlpha x || x == '_' = 
        let (ident, rest) = span (\c -> isAlphaNum c || c == '_') (x:xs)
        in if ident `elem` javaKeywords
            then Keyword ident : lexer rest
            else Identifier ident : lexer rest

-- Invalid Characters
lexer (x:xs) = Err ("Unrecognized token: " ++ [x]) : lexer xs
