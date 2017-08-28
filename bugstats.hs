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
  | str == "closed" = Token_Status Status_Closed
  | str == "open" = Token_Status Status_Closed
  | otherwise = Token_Identifier str


mkBugsFromTokens :: [Token] -> [Bug] -> [Bug]
mkBugsFromTokens [] _ = []
mkBugsFromTokens all@(token : tokenList) bugList
  | token == Token_StartBugHeader = mkBug all : bugList


mkBug :: [Token] -> Bug
mkBug (token : list)
  | token == Token_StartBugHeader = mkBugDate list


mkBugDate :: [Token] -> Bug
mkBugDate (Token_Identifier m:Token_Identifier d:Token_Identifier y :Token_Dash: list) =
  mkBugTimeSpent (list, Bug (m ++ " " ++ d ++ " "++ y) "" Status_Undefined)
mkBugDate _ = error "Invalid Header : Date"

mkBugTimeSpent :: ([Token], Bug) -> Bug
mkBugTimeSpent ( (Token_Identifier time : Token_Dash : list), Bug date _ status) = mkBugStatus (list, Bug date time status)
mkBugTimeSpent _ = error "Invalid Header : Time"

mkBugStatus :: ([Token], Bug) -> Bug
mkBugStatus ( (Token_Status status: Token_Dash: list), Bug date time _ ) = Bug date time status
mkBugStatus ( (Token_Identifier s:list), bug ) = error s
mkBugStatus _ = error "Invalid Header : Status"

mkBugDescription :: ([Token], Bug) -> Bug
mkBugDescription ((token : list), bug) = bug

data BugStatus = Status_Undefined | Status_Open | Status_Closed deriving (Show, Eq)

data Token = Token_StartBugHeader
           | Token_StartBugTag
           | Token_Dash
           | Token_Newline
           | Token_Whitespace
           | Token_Identifier String
           | Token_Status BugStatus
  deriving (Show, Eq)


data Bug = Bug { date      :: String,
                 timeSpent :: String,
                 status    :: BugStatus }
 deriving (Show)


