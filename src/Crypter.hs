{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crypter where

import Crypto.Cipher.AES
import Crypto.Error
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import Data.ByteArray (ByteArray)
import qualified Crypto.Random.Types as CRT
import Data.ByteString (ByteString)
import Crypto.Data.Padding
import Control.Arrow
import Data.Maybe



getCipher :: ByteString -> CryptoFailable AES256
getCipher = cipherInit 

encrypt :: ByteString -> ByteString -> CryptoFailable ByteString
encrypt key msg = do
    let paddedKey = pad (PKCS7 32) key
    let paddedMsg = pad (PKCS7 16) msg
    cipher <- getCipher paddedKey
    return $ ecbEncrypt cipher paddedMsg

decrypt :: ByteString -> ByteString -> CryptoFailable (Maybe ByteString)
decrypt key msg = do
    let paddedKey = pad (PKCS7 32) key
    cipher <- getCipher paddedKey
    let decrypted = ecbDecrypt cipher msg
    return $ unpad (PKCS7 16) decrypted



encryptIO, decryptIO :: ByteString -> ByteString -> IO ByteString
encryptIO key = throwCryptoErrorIO . encrypt key
decryptIO key = decrypt key >>> throwCryptoErrorIO >>> fmap (fromMaybe "Error occured in unpadding")