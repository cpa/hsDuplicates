import System.Directory
import System.FilePath
import System.Posix.Files
import System.IO
import qualified Data.ByteString as BS
import Control.Monad
import Control.Parallel
import Data.List (tails)

dir1 = "/Users/cpa/Desktop/Music"
dir2 = "/Users/cpa/Desktop/musique"
dir3 = "/Users/cpa/test"
dir4 = "/Users/cpa/fun"
dir5 = "/Users/cpa/Documents"
dir6 = "/Users/cpa/testouze"

printListFiles :: [FilePath] -> IO ()
printListFiles [] = return ()
printListFiles (f:fs) = do hSetBuffering stdout LineBuffering
                           putStrLn f
                           printListFiles fs

data TreeFile = File FilePath | Dir FilePath [TreeFile]
                deriving (Show,Eq)

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
  return $ Dir path ([File $ path </> f | f <- files]++treeDirs)

listFiles :: TreeFile -> [FilePath]
listFiles (File f) = [f]
listFiles (Dir d fs) = fs >>= listFiles

sameSize :: FilePath -> FilePath -> IO Bool
sameSize x y = do
  sx <- getFileStatus x
  sy <- getFileStatus y
  return $ fileSize sx == fileSize sy

sameContents :: FilePath -> FilePath -> IO Bool
sameContents x y = do
  hx <- openFile x ReadMode
  cx <- BS.hGetContents hx
  hClose hx
  hy <- openFile y ReadMode
  cy <- BS.hGetContents hy
  hClose hy
  return $ cx == cy

toDeleteFile :: FilePath -> [FilePath] -> IO [FilePath]
toDeleteFile _ [] = return []
toDeleteFile x l = do
  h   <- openFile x ReadMode
  cts <- BS.hGetContents h
  hClose h
  aux cts x =<< (filterM (sameSize x) l)
      where aux cts x [] = return []
            aux cts x (f:fs) = do
                     h    <- openFile f ReadMode
                     cts2 <- BS.hGetContents h
                     hClose h
                     if cts2 == cts then return [x]
                     else aux cts x fs

toDelete :: [FilePath] -> [FilePath] -> Int -> Int -> IO [FilePath]
toDelete _ []  _ _             = return []
toDelete list1 (f:fs) cur len  = do 
  putStrLn (show cur ++ "/" ++ show len ++ ": " ++ show f)
  let lol = toDeleteFile f list1
  liftM2 ((++)) (lol) (toDelete list1 fs (cur+1) len)

main :: IO ()
main = do
  tree1 <- createTree dir2
  tree2 <- createTree dir1
  let list1 = listFiles tree1
      list2 = listFiles tree2
  l <- toDelete list1 list2 1 (length list2)
  printListFiles l
--  putStrLn $ (show $ length list1) ++ " " ++ (show $ length list2)
