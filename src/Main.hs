{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString as BS
import Crypter as C
import System.Environment (getArgs)
import System.Directory
import System.IO

modes, reserved :: [String]
modes = ["-e", "-d", "-cat"]
reserved = modes ++ ["-r"]

errorOn :: Bool -> String -> IO ()
errorOn t msg = if t then ioError (userError ("[Crypter Error]" ++ msg)) else pure ()

printE :: Int -> String -> IO ()
printE n msg = putStrLn $ "[Crypter] " ++ (concat . take n $ repeat "\t") ++ msg

printC :: String -> IO ()
printC msg = putStrLn $ "[Crypter] " ++ msg

data Mode = ENCRYPT | DECRYPT | CAT
type Key = BS.ByteString


main :: IO ()
main = do
    (files, mode) <- getFilesAndMode
    key <- getKey
    mapM printC files
    mapM_ (modeToOperation mode key) files 

modeToOperation :: Mode -> Key -> FilePath -> IO ()
modeToOperation ENCRYPT = encryptFile
modeToOperation DECRYPT = decryptFile
modeToOperation CAT = cat

encryptFile, decryptFile, cat :: Key -> FilePath -> IO ()
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

cat key filePath = 
    if not (isEncrypted filePath) then printC $ "Ignoring " ++ filePath ++ ", it is not encrypted"
    else do
        printC $ "Showing " ++  cutEncrypted filePath
        file <- BS.readFile filePath
        decrypted <- C.decryptIO key file
        BS.putStrLn decrypted
    
getAll :: FilePath -> IO [FilePath]
getAll path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            printC $ "Recursively on " ++ path
            contents <- listDirectory path
            subResults <- mapM getAll $ map ((path ++ "/") ++ ) contents
            return $ fmap concat subResults
        else return [path]


getKey :: IO Key
getKey = do
    printC "Please provide the key"
    hSetEcho stdin False
    key1 <- BS.getLine
    errorOn (BS.length key1 > 32) "Please provide key of length at most 32 characters"
    printC "Please provide the again"
    key2 <- BS.getLine
    errorOn (key1 /= key2) "Keys differ"
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
    errorOn (flags == []) "Please specify a mode (d, e, cat)"
    errorOn (length flags > 1) "Please specify a mode only once"
    pure $ case head flags of
        "-e"     -> ENCRYPT
        "-d"     -> DECRYPT
        "-cat"   -> CAT

isEncrypted :: FilePath -> Bool
isEncrypted path = drop (length path - len) path == ".encrypted"
    where len = length (".encrypted" :: String)

cutEncrypted :: FilePath -> FilePath
cutEncrypted path = drop (length path - len) path
    where len = length (".encrypted" :: String)