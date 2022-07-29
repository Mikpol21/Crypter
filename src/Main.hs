{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypter
import Util

main :: IO ()
main = do
    (files, mode) <- getFilesAndMode
    key <- getKey
    displayWorkingFiles files
    mapM_ (modeToOperation mode key) files 
    printC "Done!"