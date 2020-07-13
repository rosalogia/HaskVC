import System.Process
import Text.Parsec

rawDiff :: FilePath -> FilePath -> IO String
rawDiff old new = do 
    (_,out,_) <- readProcessWithExitCode "diff" [old,new] ""
    return out

parseDiff :: IO String -> IO (Maybe Diff)
parseDiff rawDiffIO = do 
    rawDiff <- rawDiffIO 
    let eitherDiff = runParser pDiff () "" rawDiff
        result = processEither eitherDiff
    return result
    where
        processEither :: Either a b -> Maybe b
        processEither (Left e) = Nothing
        processEither (Right r) = Just r

type Identifier = (String,Char,String)
type Changes = [String]
type ChangeBlock = (Identifier,Changes)
type Diff = [ChangeBlock]

parseIdentifier :: Parsec String () Identifier
parseIdentifier = do
    firstLine <- many1 ((try digit) <|> (try $ char ','))
    indicator <- letter
    secondLine <- many1 ((try digit) <|> (try $ char ','))
    newline
    return (firstLine,indicator,secondLine)

parseChange :: Parsec String () String
parseChange = do
    oneOf ['<','>','=']
    space
    change <- many . noneOf $ ['\n']
    newline
    return change
parseDivider :: Parsec String () String
parseDivider = do
    divider <- many1 . oneOf $ ['-']
    newline
    return divider

parseChanges :: Parsec String () Changes
parseChanges = many1 (parseChange <|> parseDivider)

parseChangeBlock :: Parsec String () ChangeBlock
parseChangeBlock = do
    id <- parseIdentifier
    chs <- parseChanges
    return (id,chs)

pDiff :: Parsec String () Diff
pDiff = many parseChangeBlock
    
