import System.Directory
import System.FilePath
import System.IO
import qualified Data.ByteString as BS
import Control.Monad (liftM2)
import Control.Parallel

dir1 = "/Users/cpa/Desktop/Music"
dir2 = "/Users/cpa/Desktop/musique"
dir3 = "/Users/cpa/test"
dir4 = "/Users/cpa/fun"
dir5 = "/Users/cpa/Documents"
dir6 = "/Users/cpa/testouze"

printListFiles [] = return ()
printListFiles (f:fs) = do putStrLn f
                           printListFiles fs

data TreeFile = File FilePath | Dir FilePath [TreeFile]
                deriving (Show,Eq)

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []     =  return ([], [])
partitionM p (x:xs) =  do
  flg <- p x
  (ys, ns) <- partitionM p xs
  return (if flg then (x:ys, ns) else (ys, x:ns))

hasSameContents :: FilePath -> FilePath -> IO Bool
hasSameContents f1 f2 = do 
  -- liftM2 (==) (BS.readFile f1) (BS.readFile f2)
  h1 <- openFile f1 ReadMode
  h2 <- openFile f2 ReadMode
  c1 <- BS.hGetContents h1
  c2 <- BS.hGetContents h2
  hClose h1
  hClose h2
  return (c1 == c2)

removeUselessFiles = filter (\x -> x /= "." && x /= ".." && x /= ".DS_Store" && x /= "desktop.ini")
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

toDeleteFile :: FilePath -> [FilePath] -> IO [FilePath]
toDeleteFile _ [] = return []
toDeleteFile x l = do 
  h  <- openFile x ReadMode
  s1 <- hFileSize h
  cts <- BS.hGetContents h
  hClose h
  aux cts l s1
      where aux cts [] s1 = return []
            aux cts (f:fs) s1 = do
                     h <- openFile f ReadMode
                     s2 <- hFileSize h
                     if (s1 /= s2) then aux cts fs s1
                     else do
                       cts2 <- BS.hGetContents h
                       hClose h
                       if cts2 == cts then do  return [x]
                       else aux cts fs s1

toDelete :: [FilePath] -> [FilePath] -> Int -> Int -> IO [FilePath]
toDelete _ []  _ _             = return []
toDelete list1 (f:fs) cur len  = do 
  putStrLn (show cur ++ "/" ++ show len ++ ": " ++ show f)
  let lol = toDeleteFile f list1
  liftM2 ((++)) (lol) (toDelete list1 fs (cur+1) len)

main = do
  tree1 <- createTree dir3
  tree2 <- createTree dir6
  let list1 = listFiles tree1
      list2 = listFiles tree2
  l <- toDelete list1 list2 1 (length list2)
  printListFiles l
