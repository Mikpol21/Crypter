{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.ByteString as BS
import Crypter as C
main :: IO ()
main = do
    file <- BS.readFile "README.md"
    encrypted <- C.encryptIO "1" file
    BS.writeFile "encryptedREADME" encrypted
    decrypted <- C.decryptIO "1" encrypted
    BS.writeFile "decrytpedREADME" decrypted