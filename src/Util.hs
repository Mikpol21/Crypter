module Util where
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Directory
import System.IO

getFilesAndMode :: IO ([FilePath], Mode)
getFilesAndMode = do
    args <- getArgs
    mode <- getMode args
    let files = filter (\w -> not $ w `elem` reserved) args
    workFiles <- if "-r" `elem` args 
        then fmap concat $ mapM getAll files 
        else pure files -- TODO check for dicts
    mapM_ doesFileExist workFiles
    errorOn (files == []) "Please provide file(s) name"
    return (workFiles, mode)

getMode :: [String] -> IO Mode
getMode args = do
    let flags = filter (\w -> w `elem` modes) args
    errorOn (flags == []) "Please specify a mode (-d, -e, -c)"
    errorOn (length flags > 1) "Please specify a mode only once"
    pure $ case head flags of
        "-e"     -> ENCRYPT
        "-d"     -> DECRYPT
        "-c"     -> CAT



isEncrypted :: FilePath -> Bool
isEncrypted path = drop (length path - len) path == ".encrypted"
    where len = length (".encrypted" :: String)

cutEncrypted :: FilePath -> FilePath
cutEncrypted path = take (length path - len) path
    where len = length (".encrypted" :: String)

modes, reserved :: [String]
modes = ["-e", "-d", "-c"]
reserved = modes ++ ["-r"]

errorOn :: Bool -> String -> IO ()
errorOn t msg = if t then ioError (userError ("\ESC[95m[Crypter Error]\ESC[0m" ++ msg)) else pure ()

printE :: Int -> String -> IO ()
printE n msg = putStrLn $ "\ESC[91m[Crypter]\ESC[0m " ++ (concat . take n $ repeat "\t") ++ msg

printC :: String -> IO ()
printC msg = printE 0 msg

data Mode = ENCRYPT | DECRYPT | CAT
type Key = BS.ByteString


displayWorkingFiles :: [FilePath] -> IO ()
displayWorkingFiles files = do
    printC "Working on:"
    mapM (printE 1) files
    printC ""

getAll :: FilePath -> IO [FilePath]
getAll path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            contents <- listDirectory path
            subResults <- mapM getAll $ map ((path ++ "/") ++ ) contents
            return $ fmap concat subResults
        else return [path]
