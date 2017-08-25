import Data.Char as Char

main = do
  fileContents <- catFile "./bugs"
  let bugs = parseLinesInFile (lines fileContents)
  print bugs


catFile :: String -> IO String
catFile = readFile


parseLinesInFile :: [String] -> [Token]
parseLinesInFile [] = [Undefined]
parseLinesInFile (line : lines) = tokenizeLine line

tokenizeLine :: [Char] -> [Token]
tokenizeLine [] = []
tokenizeLine (char : charStream) = tokenizeChar char : tokenizeLine charStream

tokenizeChar :: Char -> Token
tokenizeChar c
  | c == '#' = Hash
  | c == ':' = Colon
  | c == '-' = Delim
  | c == ' ' = White
  | isDigit c = Digit
  | isAlpha c = Alpha
  | otherwise = Undefined



-- accumulate :: String -> (String, String)
-- accumulate (s, ss) = (s, ss)

-- mkBug :: String -> Bug
-- mkBug s = words s

-- mkTag :: (String, Bug) -> Bug




data Token = Hash | White | Colon | Delim | Digit | Alpha | Undefined
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





