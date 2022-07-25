{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.ByteString as BS hiding (elem, filter, head)
import Crypter as C
import System.Environment (getArgs)
import System.Directory (removeFile)
import System.IO

reserved :: [String]
reserved = ["e", "d", "cat"]

errorOn :: Bool -> String -> IO ()
errorOn t msg = if t then ioError (userError msg) else pure ()

data Mode = ENCRYPT | DECRYPT | CAT

main :: IO ()
main = do
    args <- getArgs
    mode <- getMode args
    
    let notFlags = filter (\w -> not $ w `elem` reserved) args
    errorOn (notFlags == []) "Please provide file name"
    let file = head notFlags

    Prelude.putStrLn "Please provide the key"
    hSetEcho stdin False
    key <- BS.getLine
    errorOn (BS.length key > 32) "Please provide key of length at most 32 characters"

    case mode of
        ENCRYPT -> encryptFile key file
        DECRYPT -> decryptFile key file
        CAT     -> cat key file

encryptFile, decryptFile, cat :: ByteString -> FilePath -> IO ()
encryptFile key filePath = do
    file <- BS.readFile filePath
    encrypted <- C.encryptIO key file
    BS.writeFile ("encrypted-" ++ filePath) encrypted
    removeFile filePath
    BS.writeFile (".config-" ++ filePath) key

decryptFile key filePath = do
    file <- BS.readFile encryptedPath
    decryptFileed <- C.decryptIO key file
    BS.writeFile filePath decryptFileed
    removeFile (".config-" ++ filePath)
    removeFile encryptedPath
    where 
        encryptedPath = "encrypted-" ++ filePath

cat key filePath = do
    file <- BS.readFile encryptedPath
    decrypted <- C.decryptIO key file
    BS.putStrLn decrypted
    where 
        encryptedPath = "encrypted-" ++ filePath

getMode :: [String] -> IO Mode
getMode args = do
    let flags = filter (\w -> w `elem` reserved) args
    errorOn (flags == []) "Please specify a mode (d, e, cat)"
    errorOn (Prelude.length flags > 1) "Please specify a mode only once"
    pure $ case head flags of
        "e" -> ENCRYPT
        "d" -> DECRYPT
        "cat" -> CAT
