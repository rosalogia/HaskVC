import System.Directory
import System.Process
import Data.Tree

-- These three functions are shitty nearly-identical functions
-- to the non-drawable ones. Didn't bother to make a function
-- that turns the output of the actual one into an IO (Tree String)
-- because this will probably only be used for debugging anyway
drawDirectoryTree :: FilePath -> IO ()
drawDirectoryTree path = do
    st <- drawableDirectoryToTree path
    putStrLn . drawTree $ st

drawableFileToNode :: FilePath -> IO (Tree String)
drawableFileToNode parent = do
    inode <- getInode parent
    return $ Node ((show inode) ++ ", " ++ parent) []

drawableDirectoryToTree :: FilePath -> IO (Tree String)
drawableDirectoryToTree parent = do
    -- inode <- getInode parent
    filePaths <- listDirectory parent
    let fileForestM = map (processContents parent) filePaths
    fileForest <- unwrapListMonads fileForestM
    return $ Node ((show 0) ++ ", " ++ parent) fileForest
    where
    processContents :: FilePath -> FilePath -> IO (Tree String)
    processContents parent entry = do
        let fullPath = parent++"/"++entry
        -- isFile <- doesFileExist (fullPath)
        isDirectory <- doesDirectoryExist (fullPath)
        if isDirectory then drawableDirectoryToTree fullPath else drawableFileToNode fullPath

-- Grabs inode number from a file
getInode :: FilePath -> IO Integer
getInode path = do
    output <- readProcess "ls" ["-i",path] ""
    let (inode:_) = words output
    return . read $ inode

type FileID = (Integer, String)

-- Turns file into Tree FileID. Used as end-condition
-- to recursion in directoryToTree
fileToNode :: FilePath -> IO (Tree FileID)
fileToNode parent = do
    inode <- getInode parent
    return $ Node (inode, parent) []

-- Turns directory into Tree FileID
directoryToTree :: FilePath -> IO (Tree FileID)
directoryToTree parent = do
    -- inode <- getInode parent
    
    -- Grabs the contents of the directory as a list, then maps
    -- it with function that turns each entry into its own Tree
    filePaths <- listDirectory parent
    let fileForestM = map (processContents parent) filePaths
    fileForest <- unwrapListMonads fileForestM

    -- Stores directory with inode 0
    return $ Node (0, parent) fileForest

    where
    -- Takes entry name and parent directory, concatenates to form full 
    -- path. If the entry is a directory, calls directoryToTree. Otherwise,
    -- calls fileToNode.
    processContents :: FilePath -> FilePath -> IO (Tree FileID)
    processContents parent entry = do
        let fullPath = parent++"/"++entry
        -- isFile <- doesFileExist (fullPath)
        isDirectory <- doesDirectoryExist (fullPath)
        if isDirectory then directoryToTree fullPath else fileToNode fullPath

-- Turns list containing monad-wrapped values into
-- a monad-wrapped list of values. Need this for processContents,
-- since mapping it to a list returns the former.
unwrapListMonads :: Monad m => [m a] -> m [a]
unwrapListMonads (m:ms) = do
    entry <- m
    remaining <- unwrapListMonads ms
    return (entry:remaining)
unwrapListMonads [] = return []



