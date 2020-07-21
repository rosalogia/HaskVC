module DirectoryTree (directoryToTree, getInode) where
import System.Directory
import System.Process
import Data.Tree

drawDirectoryTree :: FilePath -> IO ()
drawDirectoryTree path = do
    st <- drawableDirectoryToTree path
    putStrLn . drawTree $ st

drawableDirectoryToTree :: FilePath -> IO (Tree String)
drawableDirectoryToTree path = do
    unformated <- directoryToTree path
    return $ formatter <$> unformated
        where
        formatter :: FileID -> String
        formatter (int, st) = (show int) ++ ", " ++ st

-- Grabs inode number from a file
getInode :: FilePath -> IO Integer
getInode path = do
    output <- readProcess "stat" ["-c","%i",path] ""
    return . read $ output

listSubEntries :: FilePath -> IO [String]
listSubEntries path = do
    isDirectory <- doesDirectoryExist path
    if isDirectory then listDirectory path else return []

type FileID = (Integer, String)

-- Turns directory into Tree FileID
directoryToTree :: FilePath -> IO (Tree FileID)
directoryToTree path = do
    inode <- getInode path
    entryContents <- listSubEntries path

    let subTreesMonadic = map (subDirectoryToTree path) entryContents
    subTrees <- unwrapListMonads subTreesMonadic

    return $ Node (inode, path) subTrees

subDirectoryToTree :: FilePath -> FilePath -> IO (Tree FileID)
subDirectoryToTree pathAccum entry = do
    -- Grabs the contents of the directory as a list, then maps
    -- directoryToTree to it to turn the entries into trees
    let absPath = pathAccum ++ "/" ++ entry
    inode <- getInode absPath
    entryContents <- listSubEntries absPath

    let subTreesMonadic = map (subDirectoryToTree absPath) entryContents
    subTrees <- unwrapListMonads subTreesMonadic

    return $ Node (inode, entry) subTrees

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

pathToList :: FilePath -> [FilePath]
pathToList path = splitBySlash path "" []
    where
    splitBySlash (char:rem) word list 
        | char == '/' = splitBySlash rem "" (list ++ word)
        | (char:rem) == "" = list
        | otherwise = splitBySlash rem (word ++ [char]) list

listToPath :: [FilePath] -> FilePath
listToPath pathList = foldl ("/"++) pathList

rmMatchHead :: Eq a => [a] -> [a] -> [a]
rmMatchHead (a1:as1) (a2:as2) 
    | a1 == a2 = rmMatchHeaad as1 as2
    | otherwise = (a2:as2)

getID :: FilePath -> FileID
getID path = (getInode path, tail . pathToList $ path)

listToTree :: [FilePath] -> [FilePath] -> Tree FileID
listToTree root (entry:[]) = Node entry []
listToTree root (entry:rem) = Node (getID . listToPath $ root++entry) [listToTree (root ++ entry) rem]

extractMatchingName :: FilePath -> [Tree FileID] -> Maybe ([Tree FileID], Tree FileID)
extractMatchingName target list = helper target [] list 
    where
    helper _ _ [] = Nothing
    helper tar passed (current:left) = 
        let (Node (_,name) _) = current
        in if name == tar 
           then Just (passed++left, current)
           else helper tar (passed++[current]) left
{-
addTreeEntry :: FileID -> Tree FileID -> Tree FileID
addTreeEntry p t = addEntry unrootedPath r t
    where
    Node (_,r) _ = t
    unrootedPath = rmMatchHead (pathToList r) (pathToList p)

    addEntry (entry:rem) passed tree =
        let Node r treeList = tree
            (_, parent) = r
            extraction = extractMatchingName entry treeList
        in if extraction == Nothing
           then Node r (treeList ++ [listToTree passed (entry:rem)])
-}


