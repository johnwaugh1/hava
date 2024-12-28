module Parser where

parser :: [Token] -> Etiher String Statement
parser tokens =
    case sr [] tokens of
        [stmt] -> Left (show stmt)
        [Err msg] -> Right $ "Parse error: " ++ msg
        stack -> Right $ "Incomplete parse. Remaining stack: " ++ show stack



