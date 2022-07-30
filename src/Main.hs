{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypter
import Util
import EncryptedFile
import qualified Data.ByteString as BS

main :: IO ()
main = do
    (files, mode) <- getFilesAndMode
    if mode == HELP then help else do
        displayWorkingFiles files
        passwd <- getPassword
        mapM_ (modeToOperation mode passwd) files 
        printC "Done!"