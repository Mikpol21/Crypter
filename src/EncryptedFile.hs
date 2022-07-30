{-# LANGUAGE OverloadedStrings #-}
module EncryptedFile where

import Data.ByteString
import Parser

data EncryptedFile = EncryptedFile { 
    getHeader :: ByteString,
    getSalt :: ByteString,
    getHash :: ByteString,
    getContent :: ByteString
}


parseFile :: Parser EncryptedFile
parseFile = do
    header <- parseHeader
    salt <- parseSalt
    hash <- parseHash
    content <- parseContent
    return $ EncryptedFile header salt hash content

parseHeader :: Parser ByteString
parseHeader = do
    header <- string defaultHeader
    string "\n"
    return header

parseSalt :: Parser ByteString
parseSalt = do
    string "salt: "
    salt <- nBytes 64
    string "\n"
    return salt

parseHash :: Parser ByteString
parseHash = do
    string "hash: "
    salt <- nBytes 24
    string "\n"
    return salt

parseContent :: Parser ByteString
parseContent = do
    string "content: "
    content <- Parser.any
    return content

toPrintable :: EncryptedFile -> ByteString
toPrintable (EncryptedFile header salt hash content) =
    header `append` "\n"
    `append` "salt: " `append` salt `append` "\n"
    `append` "hash: " `append` hash `append` "\n"
    `append` "content: " `append` content

defaultHeader ::ByteString
defaultHeader = "[Crypter] This file has been encrypted\n[Crypter] Please do not modify this file, otherwise the data will be lost" :: ByteString

toFile :: ByteString -> ByteString -> ByteString -> EncryptedFile
toFile = EncryptedFile defaultHeader

-- TODO handle parser error and report it to the user (not encrypted or parse error)
readEncryptedFile :: FilePath -> IO EncryptedFile
readEncryptedFile filePath = do
    file <- Data.ByteString.readFile filePath
    return . runParser parseFile $ file

writeEncryptedFile :: FilePath -> EncryptedFile -> IO ()
writeEncryptedFile filePath encryptedFile = do
    let content = toPrintable encryptedFile
    Data.ByteString.writeFile filePath content
