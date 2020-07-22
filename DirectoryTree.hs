module DirectoryTree (directoryToTree, addTreeEntry) where
import System.Directory
import System.Process
import Data.Tree

outputTree :: IO (Tree String) -> IO ()
outputTree t = do
    tree <- t
    putStrLn . drawTree $ tree

drawDirectoryTree :: FilePath -> IO ()
drawDirectoryTree path = do
    st <- drawableDirectoryToTree path
    putStrLn . drawTree $ st

drawableDirectoryToTree :: FilePath -> IO (Tree String)
drawableDirectoryToTree path = do
    unformated <- directoryToTree path
    return unformated

listSubEntries :: FilePath -> IO [String]
listSubEntries path = do
    isDirectory <- doesDirectoryExist path
    if isDirectory then listDirectory path else return []

type FileID = String

-- Turns directory into Tree FileID
directoryToTree :: FilePath -> IO (Tree FileID)
directoryToTree path = do
    entryContents <- listSubEntries path

    let subTreesMonadic = map (subDirectoryToTree path) entryContents
    subTrees <- unwrapListMonads subTreesMonadic

    return $ Node path subTrees

subDirectoryToTree :: FilePath -> FilePath -> IO (Tree FileID)
subDirectoryToTree pathAccum entry = do
    -- Grabs the contents of the directory as a list, then maps
    -- directoryToTree to it to turn the entries into trees
    let absPath = pathAccum ++ "/" ++ entry
    entryContents <- listSubEntries absPath

    let subTreesMonadic = map (subDirectoryToTree absPath) entryContents
    subTrees <- unwrapListMonads subTreesMonadic

    return $ Node entry subTrees

-- Turns list containing monad-wrapped values into
-- a monad-wrapped list of values. Need this for directoryToTree,
-- since mapping it to a list returns the former. Dislike this
-- solution; would prefer a directoryToTree that doesn't need this.
unwrapListMonads :: Monad m => [m a] -> m [a]
unwrapListMonads (m:ms) = do
    entry <- m
    remaining <- unwrapListMonads ms
    return (entry:remaining)
unwrapListMonads [] = return []

pathToList :: FilePath -> [String]
pathToList path = splitBySlash path "" []
    where
    splitBySlash :: FilePath -> String -> [FilePath] -> [FilePath]
    splitBySlash "" word list = list ++ [word]
    splitBySlash (char:rem) word list 
        | char == '/' = splitBySlash rem "" (list ++ [word])
        | otherwise = splitBySlash rem (word ++ [char]) list

listToPath :: [FilePath] -> FilePath
listToPath pathList = foldl (++) "" $ map (\x -> "/" ++ x) pathList

rmMatchHead :: Eq a => [a] -> [a] -> [a]
rmMatchHead [] as2 = as2
rmMatchHead (a1:as1) (a2:as2) 
    | a1 == a2 = rmMatchHead as1 as2
    | otherwise = (a2:as2)

getID :: FilePath -> FileID
getID path = last . pathToList $ path

listToTree :: [FilePath] -> [FilePath] -> Tree FileID
listToTree root (entry:[]) = Node entry []
listToTree root (entry:rem) = 
    Node (entry) [listToTree (root ++ [entry]) rem]

extractMatchingName :: FilePath -> [Tree FileID] -> Maybe ([Tree FileID], Tree FileID)
extractMatchingName target list = helper target [] list 
    where
    helper _ _ [] = Nothing
    helper tar passed (current:left) = 
        let (Node name _) = current
        in if name == tar 
           then Just (passed++left, current)
           else helper tar (passed++[current]) left

addTreeEntry :: FileID -> Tree FileID -> Tree FileID
addTreeEntry p t = addEntry unrootedPath (pathToList r) t
    where
    Node r _ = t
    unrootedPath = rmMatchHead (pathToList r) (pathToList p)
    
    addEntry :: [FilePath] -> [FilePath] -> Tree FileID -> Tree FileID
    addEntry (entry:rem) passed tree =
        let Node name treeList = tree
            extracted = extractMatchingName entry treeList

            processExtraction :: Maybe ([Tree FileID], Tree FileID) ->  Tree FileID
            processExtraction Nothing = Node name $ (listToTree passed (entry:rem)):treeList
            processExtraction (Just (filteredTrees, extraction)) = 
                Node name $ (addEntry rem (passed ++ [entry]) extraction):filteredTrees 
        in processExtraction extracted


