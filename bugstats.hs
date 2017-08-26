import Data.Char as Char

main = do
  fileContents <- readFile "bugs.md"
  let tokens = tokStream fileContents
  let bugs = mkBugsFromTokens tokens []
  print bugs


tokStream :: [Char] -> [Token]
tokStream [] = []
tokStream (char : charStream) = tokChar char : tokStream charStream


tokChar :: Char -> Token
tokChar c
  | c == '#' = Token_StartBugHeader
  | c == '*' = Token_StartBugTag
  | c == '-' = Token_Dash
  | c == ' ' = Token_Whitespace
  | c == '\t' = Token_Whitespace
  | c == '\n' = Token_Newline
  | otherwise = Token_Undefined


mkBugsFromTokens :: [Token] -> [Bug] -> [Bug]
mkBugsFromTokens [] _ = []
mkBugsFromTokens all@(token : tokenList) bugList
  | token == Token_StartBugHeader = mkBug all : bugList
  -- | token == Token_StartBugTag    = mkBug tokenList : bugs ++ mkBugsFromTokens(tokenList bugs)
  -- | otherwise = mkBugsFromTokens(tokenList bugs)

mkBug :: [Token] -> Bug
mkBug (token : list)
  | token == Token_StartBugHeader = mkBugHeader (list, Bug "Start")
  | token == Token_Undefined = Bug "Undefined"
  | token == Token_Whitespace = Bug "Whitespace"
  | otherwise = Bug "Otherwise"

mkBugHeader :: ([Token], Bug) -> Bug
mkBugHeader ((token : list), bug) = bug



data Token = Token_StartBugHeader
           | Token_StartBugTag
           | Token_Dash
           | Token_Newline
           | Token_Whitespace
           | Token_Undefined
  deriving (Show, Eq)


data Bug = Bug String deriving (Show)


