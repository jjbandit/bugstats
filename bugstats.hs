import Data.Char as Char

main = do
  fileContents <- readFile "bug.stats"
  let tokens = tokenizeStream fileContents
  let bugs = lexTokens tokens []
  print bugs


tokenizeStream :: [Char] -> [Token]
tokenizeStream [] = []
tokenizeStream (char : charStream) = tokenizeChar char : tokenizeStream charStream


tokenizeChar :: Char -> Token
tokenizeChar c
  | c == '#' = Hash
  | c == '@' = At
  | c == ':' = Colon
  | c == '-' = Dash
  | c == ' ' = Whitespace
  | c == '\t' = Whitespace
  | c == '\n' = Newline
  | isDigit c = Digit c
  | isAlpha c = Alpha c
  | otherwise = Undefined c


lexTokens :: [Token] -> [Bug] -> [Bug]
lexTokens (token : tokenList) bugs
  | token == At = mkBug tokenList : bugs
  | otherwise = lexTokens tokenList bugs


mkBug :: [Token] -> Bug
mkBug _ = Bug "" ""


data Token = Hash
           | At
           | Colon
           | Dash
           | Newline
           | Whitespace
           | Digit Char
           | Alpha Char
           | Undefined Char
  deriving (Show, Eq)


data Bug =
  Bug { header :: String,
        description :: String }
  deriving (Show)


