module DirectoryTree (directoryToTree, addTreeEntry) where
import System.Directory
import System.Process
import Data.Tree

-- Debugging functions
drawDirectoryTree :: FilePath -> IO ()
drawDirectoryTree path = do
    st <- drawableDirectoryToTree path
    putStrLn . drawTree $ st

drawableDirectoryToTree :: FilePath -> IO (Tree String)
drawableDirectoryToTree path = do
    dirTree <- directoryToTree path
    let stringTree = dirTreeToTree dirTree
    return stringTree

dirTreeToTree :: DirTree -> Tree String
dirTreeToTree (File i s) = Node ((show i)++ " " ++ s) []
dirTreeToTree (Directory s []) = Node s []
dirTreeToTree (Directory s tl) = Node s (map dirTreeToTree tl)

-- Equivalent to doesDirectoryExist but returns [] for files
listSubEntries :: FilePath -> IO [String]
listSubEntries path = do
    isDirectory <- doesDirectoryExist path
    if isDirectory then listDirectory path else return []

data DirTree = Directory String [DirTree] | File Integer String

-- Turns directory into Tree FileID
directoryToTree :: FilePath -> IO DirTree
directoryToTree path = do
    entryContents <- listSubEntries path

    let subTreesMonadic = map (processNodeType path) entryContents
    subTrees <- unwrapListMonads subTreesMonadic

    return $ Directory path subTrees

subDirectoryToTree :: FilePath -> FilePath -> IO DirTree
subDirectoryToTree pathAccum entry = do
    -- Grabs the contents of the directory as a list, then maps
    -- directoryToTree to it to turn the entries into trees
    let absPath = pathAccum ++ "/" ++ entry
    entryContents <- listSubEntries absPath

    let subTreesMonadic = map (processNodeType absPath) entryContents
    subTrees <- unwrapListMonads subTreesMonadic

    return $ Directory entry subTrees

fileToTree :: FilePath -> IO DirTree
fileToTree name = return $ File 0 name

processNodeType :: FilePath -> FilePath -> IO DirTree
processNodeType path entry = do
    let absPath = path ++ "/" ++ entry
    dir <- doesDirectoryExist absPath
    if dir then subDirectoryToTree path entry else fileToTree entry

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

listToTree :: Bool -> [FilePath] -> DirTree
listToTree False (entry:[]) = File 0 entry
listToTree True  (entry:[]) = Directory entry []
listToTree f (entry:rem) = 
    Directory (entry) [listToTree f rem]

getNodeName :: DirTree -> String
getNodeName (Directory name _) = name
getNodeName (File _ name) = name

pullDir :: FilePath -> [DirTree] -> Maybe ([DirTree], DirTree)
pullDir target list = helper target [] list 
    where
    helper _ _ [] = Nothing
    helper tar passed ((File i n):left) = helper tar ((File i n):passed) left
    helper tar passed ((Directory name l):left)
        | name == tar = Just (passed ++ left, (Directory name l))
        | otherwise   = helper tar ((Directory name l):passed) left

addTreeEntry :: Bool -> FilePath -> DirTree -> DirTree
addTreeEntry filetype p t = addEntry unrootedPath t
    where
    r = getNodeName t
    unrootedPath = rmMatchHead (pathToList r) (pathToList p)
    
    addEntry :: [FilePath] -> DirTree -> DirTree
    addEntry (entry:rem) (Directory name contents) =
        let extracted = pullDir entry contents

            processExtraction :: Maybe ([DirTree], DirTree) -> DirTree
            processExtraction Nothing = Directory name $ (listToTree filetype (entry:rem)):contents
            processExtraction (Just (filteredTrees, extraction)) = 
                Directory name $ (addEntry rem extraction):filteredTrees 

        in processExtraction extracted


