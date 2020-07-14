import System.Directory
import System.Process
import Data.Tree

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

getInode :: FilePath -> IO Integer
getInode path = do
    output <- readProcess "ls" ["-i",path] ""
    let (inode:_) = words output
    return . read $ inode

type FileID = (Integer, String)

fileToNode :: FilePath -> IO (Tree FileID)
fileToNode parent = do
    inode <- getInode parent
    return $ Node (inode, parent) []

directoryToTree :: FilePath -> IO (Tree FileID)
directoryToTree parent = do
    -- inode <- getInode parent
    filePaths <- listDirectory parent
    let fileForestM = map (processContents parent) filePaths
    fileForest <- unwrapListMonads fileForestM
    return $ Node (0, parent) fileForest
    where
    processContents :: FilePath -> FilePath -> IO (Tree FileID)
    processContents parent entry = do
        let fullPath = parent++"/"++entry
        -- isFile <- doesFileExist (fullPath)
        isDirectory <- doesDirectoryExist (fullPath)
        if isDirectory then directoryToTree fullPath else fileToNode fullPath

unwrapListMonads :: Monad m => [m a] -> m [a]
unwrapListMonads (m:ms) = do
    entry <- m
    remaining <- unwrapListMonads ms
    return (entry:remaining)
unwrapListMonads [] = return []



