import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import System.IO
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

data TreeFile = File FilePath | Dir FilePath [TreeFile]
                deriving (Show,Eq)

printListFiles :: [FilePath] -> IO ()
printListFiles [] = return ()
printListFiles (f:fs) = do putStrLn f
                           printListFiles fs


partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []     =  return ([], [])
partitionM p (x:xs) =  do
  flg <- p x
  (ys, ns) <- partitionM p xs
  return (if flg then (x:ys, ns) else (ys, x:ns))

removeUselessFiles = filter (\x -> x /= "." && x /= ".." && x /= ".DS_Store" && x /= "desktop.ini") 
                   . filter (\x -> (takeExtension x) == ".mp3" 
                                || (takeExtension x) == ".wav"
                                || (takeExtension x) == ".MP3"
                                || (takeExtension x) == ".avi"
                                || (takeExtension x) == ".m4a" 
                                || (takeExtension x) == ".wma"
                                || (takeExtension x) == "")
sanitize = removeUselessFiles

createTree :: FilePath -> IO TreeFile
createTree path = do
  cts          <- getDirectoryContents path
  (dirs,files) <- partitionM (\f -> doesDirectoryExist (path </> f)) $ sanitize cts
  treeDirs     <- mapM createTree (map (path </>) dirs)
  return $ Dir path ([File $ path </> f | f <- files] ++ treeDirs)

listFiles :: TreeFile -> [FilePath]
listFiles (File f) = [f]
listFiles (Dir d fs) = fs >>= listFiles

-- Matches a file against a list of files
toDeleteFile :: (FilePath,Int) -> [(FilePath,Int)] -> IO [FilePath]
toDeleteFile _ [] = return []
toDeleteFile (f,fsize) l = do
  let possibleMatches = [ x | (x,xsize) <- l, xsize == fsize ]
  if possibleMatches == [] then return [] else do
      h   <- openFile f ReadMode
      cts <- BS.hGetContents h
      hClose h
      aux cts f possibleMatches
    where aux cts x [] = return []
          aux cts x (f:fs) = 
              do h    <- openFile f ReadMode
                 cts2 <- BS.hGetContents h
                 hClose h
                 if cts2 == cts then return [x]
                 else aux cts x fs

-- Matches a list of files against another
type FilesState = StateT (Int,Int) IO
toDelete :: [(FilePath,Int)] -> [(FilePath,Int)] -> FilesState [FilePath]
toDelete _ [] = return $ return []
toDelete list1 ((f,fsize):fs) = do
  (cur,len) <- get
  liftIO $ putStrLn (show cur ++ "/" ++ show len ++ ": " ++ f)
  put (cur+1,len)
  liftM2 (++) (liftIO $ toDeleteFile (f,fsize) list1) (toDelete list1 fs)

addSize :: [FilePath] -> IO [(FilePath,Int)]
addSize [] = return []
addSize (f:fs)  = do
  sf <- getFileStatus f
  next <- addSize fs
  return $ (f,(fromIntegral $ fileSize sf)):next

main :: IO ()
main = do
  dir1:dir2:_ <- getArgs
  hSetBuffering stdout LineBuffering
  putStrLn $ "Listing files from: " ++ (show dir1)
  tree1 <- createTree dir1
  putStrLn $ "Listing files from: " ++ (show dir2)
  tree2 <- createTree dir2
  putStrLn $ "Calculating files size from: " ++ (show dir1)
  list1 <- addSize $ listFiles tree1
  putStrLn $ "Calculating files size from: " ++ (show dir2)
  list2 <- addSize $ listFiles tree2
  putStrLn $ "Matching files..."
  filesToDelete <- evalStateT (toDelete list1 list2) (1,length list2)
  printListFiles filesToDelete
  mapM_ removeFile filesToDelete
