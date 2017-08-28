import Data.Char as Char

main = do
  fileContents <- readFile "bugs.md"
  let tokens = tokenizeWords (words fileContents)
  let bugs = mkBugsFromTokens tokens []
  printBugs bugs

printBugs :: [Bug] -> IO ()
printBugs [] = print "Done"
printBugs (bug : bugs) = do
  print bug
  printBugs bugs

tokenizeWords :: [String] -> [Token]
tokenizeWords [] = []
tokenizeWords (char : charStream) = tokenizeString char : tokenizeWords charStream


tokenizeString :: String -> Token
tokenizeString str@(c : tail)
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
mkBugsFromTokens [] bugs = bugs
mkBugsFromTokens tokens@(token : remainingTokens) bugs
  | token == Token_StartBugHeader = do
     let tempBugs = mkBug tokens : bugs
     mkBugsFromTokens remainingTokens tempBugs
  | otherwise = mkBugsFromTokens remainingTokens bugs


mkBug :: [Token] -> Bug
mkBug (token : list)
  | token == Token_StartBugHeader = popDateTokens list


popDateTokens :: [Token] -> Bug
popDateTokens (Token_Identifier m:Token_Identifier d:Token_Identifier y :Token_Dash: list) =
  popTimeTokens (list, Bug (m ++ " " ++ d ++ " "++ y) "" Status_Undefined "" )
popDateTokens _ = error "Invalid Header : Date"

popTimeTokens :: ([Token], Bug) -> Bug
popTimeTokens ( (Token_Identifier time : Token_Dash : list), Bug date _ status title ) =
  popStatusTokens (list, Bug date time status title)
popTimeTokens _ = error "Invalid Header : Time"

popStatusTokens :: ([Token], Bug) -> Bug
popStatusTokens ( (Token_Status status: Token_Dash: list), Bug date time _ title ) =
  mkBugTitle (list, Bug date time status title)
popStatusTokens _ = error "Invalid Header : Status"

mkBugTitle :: ([Token], Bug) -> Bug
mkBugTitle ((Token_Identifier word : list), Bug d t s title ) =
  mkBugTitle (list, Bug d t s (title ++ word ++ " "))
mkBugTitle (_, bug) = bug

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
                 status    :: BugStatus,
                 title     :: String }
 deriving (Show)


