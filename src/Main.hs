{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypter
import Util
import EncryptedFile
import qualified Data.ByteString as BS

main :: IO ()
main = do
    (files, mode) <- getFilesAndMode
    passwd <- getPassword
    displayWorkingFiles files
    mapM_ (modeToOperation mode passwd) files 
    printC "Done!"