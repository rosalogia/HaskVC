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


