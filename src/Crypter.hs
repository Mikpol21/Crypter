{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crypter where

import Crypto.Cipher.AES
import Crypto.Error
import Crypto.Random
import Crypto.KDF.PBKDF2
import Crypto.Hash
import Crypto.Hash.Algorithms
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import Data.ByteArray (ByteArray)
import qualified Data.ByteString as BS
import Crypto.Data.Padding
import Control.Arrow
import Data.Maybe
import Util
import System.Directory
import System.IO
import Data.ByteString.Char8 (putStr)
import EncryptedFile
import Data.ByteArray (unpack)

type ByteString = BS.ByteString


deriveKey :: ByteString -> ByteString -> ByteString
deriveKey password salt = fastPBKDF2_SHA256 (Parameters 4000 32) password salt

getCipher :: ByteString -> CryptoFailable AES256
getCipher = cipherInit 

encrypt :: ByteString -> ByteString -> CryptoFailable ByteString
encrypt key msg = do
    let paddedMsg = pad (PKCS7 16) msg
    cipher <- getCipher key
    return $ ecbEncrypt cipher paddedMsg

decrypt :: ByteString -> ByteString -> CryptoFailable (Maybe ByteString)
decrypt key msg = do
    cipher <- getCipher key
    let decrypted = ecbDecrypt cipher msg
    return $ unpad (PKCS7 16) decrypted


tiger :: ByteString -> ByteString
tiger bs =  BS.pack . Data.ByteArray.unpack $ (hash bs :: Digest Tiger)

encryptIO, decryptIO :: ByteString -> ByteString -> IO ByteString
encryptIO key = throwCryptoErrorIO . encrypt key
decryptIO key = decrypt key >>> throwCryptoErrorIO >>> fmap (fromMaybe "Error occured in unpadding")


encryptFile, decryptFile, catFile :: ByteString -> FilePath -> IO ()

encryptFile passwd filePath = do -- TODO do not encrypt already encrypted files
    printC $ "Encrypting " ++ filePath
    salt <- generateSalt
    let key = deriveKey passwd salt
    let hashedKey = tiger key

    file <- BS.readFile filePath
    content <- encryptIO key file
    let encryptedFile = toFile salt hashedKey content
    
    writeEncryptedFile (filePath ++ ".encrypted") encryptedFile
    removeFile filePath
    renameFile (filePath ++ ".encrypted") filePath

decryptFile passwd filePath = do -- TODO Handle parser errors
    printC $ "Decrypting " ++ filePath
    encryptedFile <- readEncryptedFile filePath
    let key = deriveKey passwd $ getSalt encryptedFile
    passed <- safetyCheck (getHash encryptedFile) key filePath
    if passed then do
        decryptedContents <- decryptIO key $ getContent encryptedFile
        BS.writeFile (filePath ++ ".decrypted") decryptedContents
        removeFile filePath
        renameFile (filePath ++ ".decrypted") filePath
    else pure ()

catFile passwd filePath = do
    printC $ "Showing " ++  filePath
    encryptedFile <- readEncryptedFile filePath
    let key = deriveKey passwd $ getSalt encryptedFile
    decrypted <- decryptIO key $ getContent encryptedFile
    Data.ByteString.Char8.putStr decrypted



modeToOperation :: Mode -> Key -> FilePath -> IO ()
modeToOperation ENCRYPT = encryptFile
modeToOperation DECRYPT = decryptFile
modeToOperation CAT = catFile


getPassword :: IO Key
getPassword = do
    printC "Please provide password:"
    hSetEcho stdin False
    password1 <- BS.getLine
    printC "Please provide password again"
    password2 <- BS.getLine
    hSetEcho stdin True
    errorOn (password1 /= password2) "passwords differ"
    return password1

generateSalt :: IO ByteString
generateSalt = getRandomBytes 64


safetyCheck :: ByteString -> ByteString -> String -> IO Bool
safetyCheck hsh key filePath =
    let hashedKey = tiger key in
    if hashedKey == hsh then pure True else do
        printC $ "Given password is wrong or file " ++ filePath ++ " has been modified"
        printC $ "Are you sure you want to decrypt " ++ filePath ++ "? (y/N)"
        response <- getLine
        if response == "y" then pure True else 
            (printC $ "Ignoring " ++ filePath) >> pure False