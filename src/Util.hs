module Util where
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Directory
import System.IO

getFilesAndMode :: IO ([FilePath], Mode)
getFilesAndMode = do
    args <- getArgs
    mode <- getMode args
    if mode == HELP then pure ([], HELP) else do 
        let files = filter (\w -> not $ w `elem` reserved) args
        workFiles <- if "-r" `elem` args 
            then fmap concat $ mapM getAll files 
            else pure files -- TODO check for dicts
        mapM_ doesFileExist workFiles
        errorOn (files == []) "Please provide file(s) name"
        return (workFiles, mode)

getMode :: [String] -> IO Mode
getMode args =
    if "-help" `elem` args || "-h" `elem` args then pure HELP 
    else do
        let flags = filter (\w -> w `elem` modes) args
        errorOn (flags == []) "Please specify a mode (-d, -e, -c)"
        errorOn (length flags > 1) "Please specify a mode only once"
        pure $ case head flags of
            "-e"     -> ENCRYPT
            "-d"     -> DECRYPT
            "-c"     -> CAT
            "-help"  -> HELP



isEncrypted :: FilePath -> Bool
isEncrypted path = drop (length path - len) path == ".encrypted"
    where len = length (".encrypted" :: String)

cutEncrypted :: FilePath -> FilePath
cutEncrypted path = take (length path - len) path
    where len = length (".encrypted" :: String)

modes, reserved :: [String]
modes = ["-e", "-d", "-c", "-h", "-help"]
reserved = modes ++ ["-r"]

errorOn :: Bool -> String -> IO ()
errorOn t msg = if t then ioError (userError ("\ESC[95m[Crypter Error]\ESC[0m" ++ msg)) else pure ()

printE :: Int -> String -> IO ()
printE n msg = putStrLn $ "\ESC[91m[Crypter]\ESC[0m " ++ (concat . take n $ repeat "\t") ++ msg

printC :: String -> IO ()
printC msg = printE 0 msg

data Mode = ENCRYPT | DECRYPT | CAT | HELP deriving (Eq, Show)
type Key = BS.ByteString


displayWorkingFiles :: [FilePath] -> IO ()
displayWorkingFiles files = do
    printC "Working on:"
    mapM (printE 1) files
    pure ()

getAll :: FilePath -> IO [FilePath]
getAll path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            contents <- listDirectory path
            subResults <- mapM getAll $ map ((path ++ "/") ++ ) contents
            return $ fmap concat subResults
        else return [path]


help :: IO ()
help = do
    printC "Usage: ./Crypter flags files"
    printC "Flags: "
    printE 1 "-e -> encrypt file"
    printE 1 "-c -> show contents of encrypted file"
    printE 1 "-d -> decrypt encrypted file"
    printE 1 "-r -> work recursively on directories"
    printE 1 "-h -> guide"