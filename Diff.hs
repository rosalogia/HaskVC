import System.Process
import Text.Parsec

module Diff (rawDiff, assembleDiff, parseDiff) where

rawDiff :: FilePath -> FilePath -> IO String
rawDiff old new = do 
    (_,out,_) <- readProcessWithExitCode "diff" [old,new] ""
    return out

assembleDiff :: Diff -> IO String
assembleDiff diff = return $ concatStrings mapped
    where
    concatStrings (x:[]) = x
    concatStrings (x:xs) = x ++ (concatStrings xs)

    mapped = fmap concatBlock diff
    concatBlock ((ln1,ind,ln2),changes)
        | ind == 'd' = ln1 ++ [ind] ++ ln2 ++ "\n" ++ (handleDel changes) ++ "\n"
        | ind == 'c' = ln1 ++ [ind] ++ ln2 ++ "\n" ++ (handleChanges changes "< ") ++ "\n"
        | ind == 'a' = ln1 ++ [ind] ++ ln2 ++ "\n" ++ (handleAdd changes) ++ "\n"
        | otherwise  = ""
    
    handleDel (x:[]) = "< " ++ x
    handleDel (x:xs) = "< " ++ x ++ "\n" ++ (handleDel xs)  

    handleAdd (x:[]) = "> " ++ x
    handleAdd (x:xs) = "> " ++ x ++ "\n" ++ (handleAdd xs)

    handleChanges (x:[]) prefix = prefix ++ x
    handleChanges (x:xs) prefix = if x=="---" 
                                  then (handleChanges xs "> ") 
                                  else (prefix ++ x ++ "\n" ++ (handleChanges xs prefix))

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
    

