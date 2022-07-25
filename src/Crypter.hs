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




getCipher :: ByteString -> CryptoFailable AES256
getCipher = cipherInit 

encrypt, decrypt :: ByteString -> ByteString -> CryptoFailable ByteString
encrypt key msg = do
    cipher <- getCipher key
    return $ ecbEncrypt cipher msg

decrypt key msg = do
    cipher <- getCipher key
    return $ ecbDecrypt cipher msg
