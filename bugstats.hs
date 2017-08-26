import Data.Char as Char

main = do
  fileContents <- readFile "bugs.md"
  let tokens = tokStream (words fileContents)
  let bugs = mkBugsFromTokens tokens []
  print bugs


tokStream :: [String] -> [Token]
tokStream [] = []
tokStream (char : charStream) = tokStr char : tokStream charStream


tokStr :: String -> Token
tokStr str@(c : tail)
  | c == '#' = Token_StartBugHeader
  | c == '*' = Token_StartBugTag
  | c == '-' = Token_Dash
  | c == ' ' = Token_Whitespace
  | c == '\t' = Token_Whitespace
  | c == '\n' = Token_Newline
  | otherwise = Token_Identifier str


mkBugsFromTokens :: [Token] -> [Bug] -> [Bug]
mkBugsFromTokens [] _ = []
mkBugsFromTokens all@(token : tokenList) bugList
  | token == Token_StartBugHeader = mkBug all : bugList


mkBug :: [Token] -> Bug
mkBug (token : list)
  | token == Token_StartBugHeader = mkBugHeader list


mkBugHeader :: [Token] -> Bug
mkBugHeader (Token_Identifier m:Token_Identifier d:Token_Identifier y:Token_Dash:Token_Identifier time:Token_Dash:Token_Identifier status: list) =
  Bug (m ++ " " ++ d ++ " "++ y) time status
mkBugHeader _ = error "Invalid Header"

mkBugDescription :: ([Token], Bug) -> Bug
mkBugDescription ((token : list), bug) = bug



data Token = Token_StartBugHeader
           | Token_StartBugTag
           | Token_Dash
           | Token_Newline
           | Token_Whitespace
           | Token_Identifier String
  deriving (Show, Eq)


data Bug = Bug { date      :: String,
                 timeSpent :: String,
                 status    :: String }
 deriving (Show)


