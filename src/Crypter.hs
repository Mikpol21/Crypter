{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crypter where

import Crypto.Cipher.AES
import Crypto.Error
import Crypto.KDF.PBKDF2
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import Data.ByteArray (ByteArray)
import qualified Crypto.Random.Types as CRT
import qualified Data.ByteString as BS
import Crypto.Data.Padding
import Control.Arrow
import Data.Maybe
import Util
import System.Directory
import System.IO
import Data.ByteString.Char8 (putStrLn)

type ByteString = BS.ByteString


deriveKey :: ByteString -> ByteString
deriveKey password = fastPBKDF2_SHA256 (Parameters 4000 32) password ("1234567890" :: ByteString)

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



encryptIO, decryptIO :: ByteString -> ByteString -> IO ByteString
encryptIO key = throwCryptoErrorIO . encrypt key
decryptIO key = decrypt key >>> throwCryptoErrorIO >>> fmap (fromMaybe "Error occured in unpadding")


encryptFile, decryptFile, catFile :: Key -> FilePath -> IO ()

encryptFile key filePath =
    if isEncrypted filePath then printC $ "Ignoring " ++ filePath ++ ", it is already encrypted"
    else do
        printC $ "Encrypting " ++ filePath
        file <- BS.readFile filePath
        encrypted <- encryptIO key file
        BS.writeFile (filePath ++ ".encrypted") encrypted
        removeFile filePath
    -- BS.writeFile (".config-" ++ filePath) key

decryptFile key filePath =
    if not (isEncrypted filePath) then printC $ "Ignoring " ++ filePath ++ ", it is not encrypted"
    else do
        printC $ "Decrypting " ++ cutEncrypted filePath
        file <- BS.readFile filePath
        decrypted <- decryptIO key file
        BS.writeFile (cutEncrypted filePath) decrypted
        -- removeFile (".config-" ++ filePath)
        removeFile filePath

catFile key filePath = 
    if not (isEncrypted filePath) then printC $ "Ignoring " ++ filePath ++ ", it is not encrypted"
    else do
        printC $ "Showing " ++  cutEncrypted filePath
        file <- BS.readFile filePath
        decrypted <- decryptIO key file
        Data.ByteString.Char8.putStrLn decrypted



modeToOperation :: Mode -> Key -> FilePath -> IO ()
modeToOperation ENCRYPT = encryptFile
modeToOperation DECRYPT = decryptFile
modeToOperation CAT = catFile


getKey :: IO Key
getKey = do
    printC "Please provide password:"
    hSetEcho stdin False
    password1 <- BS.getLine
    -- errorOn (BS.length key1 > 32) "Please provide key of length at most 32 characters"
    printC "Please provide password again"
    password2 <- BS.getLine
    errorOn (password1 /= password2) "passwords differ"
    printC "Deriving key"
    return $ deriveKey password1