import Data.Char as Char

main = do
  fileContents <- catFile "./bugs"
  let bugTokens = tokenizeStream fileContents
  print bugTokens


catFile :: String -> IO String
catFile = readFile


tokenizeStream :: [Char] -> [Token]
tokenizeStream [] = []
tokenizeStream (char : charStream) = tokenizeChar char : tokenizeStream charStream


tokenizeChar :: Char -> Token
tokenizeChar c
  | c == '#' = Hash
  | c == ':' = Colon
  | c == '-' = Delim
  | c == ' ' = Whitespace
  | c == '\n' = Newline
  | isDigit c = Digit c
  | isAlpha c = Alpha c
  | otherwise = Undefined c



-- accumulate :: String -> (String, String)
-- accumulate (s, ss) = (s, ss)

-- mkBug :: String -> Bug
-- mkBug s = words s

-- mkTag :: (String, Bug) -> Bug




data Token = Hash
           | Newline
           | Whitespace
           | Colon
           | Delim
           | Digit Char
           | Alpha Char
           | Undefined Char
  deriving (Show)


data BugHeader =
  BugHeader { date   :: String,
              hours  :: String,
              status :: String,
              title  :: String }
  deriving (Show)


data Bug =
  Bug { header :: BugHeader,
        desc   :: String,
        tags   :: [String] }
  deriving (Show)





