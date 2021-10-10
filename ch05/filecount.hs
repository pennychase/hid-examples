import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.Directory.Extra (doesDirectoryExist, listContents, listFilesRecursive)
import Control.Monad.Extra (whenM, ifM, zipWithM_)
import Data.IORef (newIORef, modifyIORef', readIORef)

fileCount :: FilePath -> IO Int
fileCount fpath = do
   counter <- newIORef 0
   whenM (doesDirectoryExist fpath) $ go counter fpath
   readIORef counter
 where
   go cnt fp = listContents fp >>= traverse_ (processEntry cnt)
   processEntry cnt fp = ifM (doesDirectoryExist fp) (go cnt fp) (inc cnt)
   inc cnt = modifyIORef' cnt (+ 1)

main :: IO ()
main = do
   args <- getArgs
   xs <- traverse fileCount args
   zipWithM_ printEntry args xs
 where
   printEntry fp n = putStrLn (show n ++ "\t" ++ fp)

-- Count files without IORef
fileCount' :: FilePath -> IO Int
fileCount' fp = length <$> listFilesRecursive fp

main' :: IO ()
main' = do
    args <- getArgs
    -- xs <- traverse fileCount' args
    zipWithM_ printEntry args (traverse fileCount' args)
  where
    printEntry fp n = putStrLn (show n ++ "\t" ++ fp)