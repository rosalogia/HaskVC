module DirectoryTree (directoryToTree, addTreeEntry, deleteTreeEntry) where
import System.Directory
import System.Process
import Data.Tree
import Data.Time
import Data.Time.LocalTime
import Data.List.Split
import qualified Data.List (map)

-- Debugging functions
drawDirectoryTree :: FilePath -> IO ()
drawDirectoryTree path = do
    tree <- directoryToTree path
    putStrLn . drawTree . dirTreeToTree $ tree

dirTreeToTree :: DirTree -> Tree String
dirTreeToTree (File i s) = Node ((show i)++ " " ++ s) []
dirTreeToTree (Directory s []) = Node s []
dirTreeToTree (Directory s tl) = Node s (map dirTreeToTree tl)

-- Turns list containing IO wrapped values into
-- a IO wrapped list of values. 
unwrapListMonads :: [IO a] -> IO [a]
unwrapListMonads (m:ms) = do
    entry <- m
    remaining <- unwrapListMonads ms
    return (entry:remaining)
unwrapListMonads [] = return []

-- Directory tree data structure
data DirTree = Directory String [DirTree] | File Integer String deriving (Show)


lastModified :: FilePath -> IO LocalTime
lastModified fpath = do 
    (_,out,_) <- readProcessWithExitCode "stat" [fpath, "-c %y"] ""

    let (dateString:timeString:_) = words out
    
    let timestamp = head $ splitOn "." $ timeString

    parsedDate <- parseTimeM True defaultTimeLocale "%Y-%0m-%0d" dateString :: IO Day

    parsedTimeOfDay <- parseTimeM True defaultTimeLocale "%T" timestamp :: IO TimeOfDay

    return $ LocalTime parsedDate parsedTimeOfDay


-- Compares two directory trees for changes
compareDirectoryTrees :: LocalTime -> DirTree -> FilePath -> IO ([String], [String], [String])
compareDirectoryTrees lastCommit commitTree currentRoot = currentTree >>= runComparison commitTree 
    where
        currentTree = directoryToTree currentRoot

        reduceUpdateTriples :: [([String], [String], [String])] -> ([String], [String], [String])
        reduceUpdateTriples triples = foldl (\(a1,b1,c1) (a2,b2,c2) -> (a1++a2,b1++b2,c1++c2)) ([],[],[]) triples

        runComparison :: DirTree -> DirTree -> IO ([String], [String], [String])
        runComparison (File id1 name1) (File id2 name2) = do
            fileModDate <- lastModified name2
            if name1 == name2 && lastCommit >= fileModDate then
                return ([], [], [])
            else if name1 == name2 && lastCommit < fileModDate then
                return ([], [name2], [])
            else
                return ([name2], [], [name1])

        runComparison (Directory path1 contents1) (Directory path2 contents2) = do
            contentComparisons <- unwrapListMonads $ Data.List.map (uncurry runComparison) (zip contents1 contents2)
            return . reduceUpdateTriples $ contentComparisons

-- return :: a -> IO a
-- map :: ([([String], [String], [String])] -> ([String], [String], [String])) -> ([([String], [String], [String])] -> IO ([String], [String], [String]))
-- List.map :: (DirTree, DirTree -> IO ([String], [String], [String])) -> [DirTree, DirTree] -> [IO ([String], [String], [String])]
-- uncurry :: (DirTree -> DirTree -> IO ([String], [String], [String])) -> (DirTree, DirTree -> ...)

-- Turns directory into Tree FileID
directoryToTree :: FilePath -> IO DirTree
directoryToTree path = entryToTree path ""
    where
    -- Process whether given path is a file or directory
    entryToTree :: FilePath -> FilePath -> IO DirTree
    entryToTree path entry = do
        let absPath = path ++ "/" ++ entry
        dir <- doesDirectoryExist absPath
        if dir then subDirectoryToTree path entry else fileToTree entry

    subDirectoryToTree :: FilePath -> FilePath -> IO DirTree
    subDirectoryToTree pathAccum entry = do
        -- Grabs the contents of the directory as a list, then maps
        -- directoryToTree to it to turn the entries into trees
        let absPath = pathAccum ++ "/" ++ entry
        entryContents <- listSubEntries absPath

        let subTreesMonadic = map (entryToTree absPath) entryContents
        subTrees <- unwrapListMonads subTreesMonadic

        return $ Directory entry subTrees

    fileToTree :: FilePath -> IO DirTree
    fileToTree name = return $ File 0 name

    -- Equivalent to doesDirectoryExist but returns [] for files
    listSubEntries :: FilePath -> IO [String]
    listSubEntries path = do
        isDirectory <- doesDirectoryExist path
        if isDirectory then listDirectory path else return []

    
-- Turns path string into list of entries
-- i.e. pathToList "/home/user" == ["home","user]
pathToList :: FilePath -> [String]
pathToList path = splitBySlash path "" []
    where
    splitBySlash :: FilePath -> String -> [FilePath] -> [FilePath]
    splitBySlash "" word list = list ++ [word]
    splitBySlash (char:rem) word list 
        | char == '/' && list == [] = splitBySlash rem "" list
        | char == '/' = splitBySlash rem "" (list ++ [word])
        | otherwise = splitBySlash rem (word ++ [char]) list

-- Adds entry to tree. First filepath is the absolute
-- path of the directory described by the given tree.
-- This is used to determine if added entry is a file
-- or a directory
addTreeEntry :: Bool -> FilePath -> DirTree -> DirTree
addTreeEntry ftype p t = addEntry (pathToList p) t
    where
    addEntry :: [FilePath] -> DirTree -> DirTree
    addEntry (entry:rem) (Directory name contents) =
        let processExtraction :: Maybe ([DirTree], DirTree) -> DirTree
            processExtraction Nothing = Directory name $ (listToTree (entry:rem)):contents
            processExtraction (Just (filteredTrees, extraction)) = 
                Directory name $ (addEntry rem extraction):filteredTrees 
        in processExtraction $ pullDir entry contents

    -- Turns a path list into a DirTree
    listToTree :: [FilePath] -> DirTree
    listToTree (entry:[])  = if ftype then Directory entry [] else File 0 entry
    listToTree (entry:rem) = 
        Directory (entry) [listToTree rem]

    -- Extracts a directory from a list of DirTree if it has
    -- the target name. Equals nothing if no match is found,
    -- ignores files.
    pullDir :: FilePath -> [DirTree] -> Maybe ([DirTree], DirTree)
    pullDir target list = helper target [] list 
        where
        helper _ _ [] = Nothing
        helper tar passed ((File i n):left) = helper tar ((File i n):passed) left
        helper tar passed ((Directory name l):left)
            | name == tar = Just (passed ++ left, (Directory name l))
            | otherwise   = helper tar ((Directory name l):passed) left

-- Deletes entry from tree. Returns nothing if entry isn't part of tree
deleteTreeEntry :: FilePath -> DirTree -> Maybe DirTree
deleteTreeEntry p t = delEntry (pathToList p) t
    where
    delEntry :: [FilePath] -> DirTree -> Maybe DirTree
    delEntry (entry:[])  (Directory name contents) = do
        (filteredTrees, extraction) <- pullEntry entry contents
        return $ Directory name filteredTrees

    delEntry (entry:rem) (Directory name contents) = do
        (filteredTrees, extraction) <- pullEntry entry contents
        delTree <- delEntry rem extraction
        return $ Directory name $ (delTree):filteredTrees

    -- Extracts an entry from a list of DirTree similarly
    -- to pullDir, but is capable of extracitng a file
    pullEntry :: FilePath -> [DirTree] -> Maybe ([DirTree], DirTree)
    pullEntry target list = helper target [] list 
        where
        helper _ _ [] = Nothing
        helper tar passed (entry:left)
            | getNodeName entry == tar = Just (passed ++ left, entry)
            | otherwise                = helper tar (entry:passed) left

    -- Get the name of a DirTree
    getNodeName :: DirTree -> String
    getNodeName (Directory name _) = name
    getNodeName (File _ name) = name



