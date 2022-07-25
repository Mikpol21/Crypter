{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.ByteString as BS hiding (elem, filter, head, putStrLn)
import Crypter as C
import System.Environment (getArgs)
import System.Directory (removeFile)
import System.IO

reserved :: [String]
reserved = ["e", "d"]

errorOn :: Bool -> String -> IO ()
errorOn t msg = if t then ioError (userError msg) else pure ()

main :: IO ()
main = do
    args <- getArgs
    errorOn ("e" `elem` args && "d" `elem` args ) "You cannot encrypt and decrypt"
    
    let files = filter (\w -> not $ w `elem` reserved) args
    errorOn (files == []) "Please provide file name"
    let file = head files

    putStrLn "Please provide the key"
    hSetEcho stdin False
    key <- BS.getLine
    errorOn (BS.length key > 32) "Please provide key of length at most 32 characters"

    if "e" `elem` args
        then encryptBranch key file
    else if "d" `elem` args
        then decryptBranch key file
    else
        ioError . userError $ "Please specify intentions using d or e"

encryptBranch, decryptBranch :: ByteString -> FilePath -> IO ()
encryptBranch key filePath = do
    file <- BS.readFile filePath
    encrypted <- C.encryptIO key file
    BS.writeFile ("encrypted-" ++ filePath) encrypted
    removeFile filePath
    BS.writeFile (".config-" ++ filePath) key

decryptBranch key filePath = do
    file <- BS.readFile encryptedPath
    decrypted <- C.decryptIO key file
    BS.writeFile filePath decrypted
    removeFile (".config-" ++ filePath)
    removeFile encryptedPath
    where 
        encryptedPath = "encrypted-" ++ filePath


