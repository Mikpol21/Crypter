{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString as BS
import Crypter as C
import System.Environment (getArgs)
import System.Directory
import System.IO

main :: IO ()
main = do
    (files, mode) <- getFilesAndMode
    key <- getKey
    displayWorkingFiles files
    mapM_ (modeToOperation mode key) files 
    printC "Done!"

--------------------------------------------------------------------

encryptFile, decryptFile, catFile :: Key -> FilePath -> IO ()
encryptFile key filePath =
    if isEncrypted filePath then printC $ "Ignoring " ++ filePath ++ ", it is already encrypted"
    else do
        printC $ "Encrypting " ++ filePath
        file <- BS.readFile filePath
        encrypted <- C.encryptIO key file
        BS.writeFile (filePath ++ ".encrypted") encrypted
        removeFile filePath
    -- BS.writeFile (".config-" ++ filePath) key

decryptFile key filePath =
    if not (isEncrypted filePath) then printC $ "Ignoring " ++ filePath ++ ", it is not encrypted"
    else do
        printC $ "Decrypting " ++ cutEncrypted filePath
        file <- BS.readFile filePath
        decrypted <- C.decryptIO key file
        BS.writeFile (cutEncrypted filePath) decrypted
        -- removeFile (".config-" ++ filePath)
        removeFile filePath

catFile key filePath = 
    if not (isEncrypted filePath) then printC $ "Ignoring " ++ filePath ++ ", it is not encrypted"
    else do
        printC $ "Showing " ++  cutEncrypted filePath
        file <- BS.readFile filePath
        decrypted <- C.decryptIO key file
        BS.putStrLn decrypted
        printC $ "End of " ++ cutEncrypted filePath
        printC ""

--------------------------------------------------------------------

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


getKey :: IO Key
getKey = do
    printC ""
    printC "Please provide the key:"
    hSetEcho stdin False
    key1 <- BS.getLine
    errorOn (BS.length key1 > 32) "Please provide key of length at most 32 characters"
    printC "Please provide the again"
    key2 <- BS.getLine
    errorOn (key1 /= key2) "Keys differ"
    printC ""
    return key1


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
    errorOn (flags == []) "Please specify a mode (d, e, catFile)"
    errorOn (length flags > 1) "Please specify a mode only once"
    pure $ case head flags of
        "-e"     -> ENCRYPT
        "-d"     -> DECRYPT
        "-c"     -> CAT


modeToOperation :: Mode -> Key -> FilePath -> IO ()
modeToOperation ENCRYPT = encryptFile
modeToOperation DECRYPT = decryptFile
modeToOperation CAT = catFile

--------------------------------------------------------------------

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