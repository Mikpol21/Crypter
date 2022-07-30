{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

-- import Data.Word8(isDigit,isSpace,ord,chr)
import Control.Applicative ( Alternative(..), liftA2 )
import Control.Monad ( MonadPlus(..) )
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word

--------  Parser instances  --------

newtype Parser a = Parser { parse :: ByteString -> [(a, ByteString)] }
runParser :: Parser a -> ByteString -> a
runParser m s = case complete of
    x:xs -> fst x
    []   -> if not . Prelude.null $ res 
            then error "Parser did not consume entire stream."
            else error $ "Parser error:" ++ (show $ map snd res)
    where   res = parse (spaces >> m) s
            complete = Prelude.filter (BS.null . snd) res

instance Monad Parser where
    return = pure
    Parser p >>= f = Parser $ \xs -> do
        (a, ys) <- p xs
        f a `parse` ys

instance Functor Parser where
    fmap f (Parser p) = Parser (\xs -> [(f x, xs) | (x, xs) <- p xs])

instance Applicative Parser where
    pure a = Parser $ \xs -> [(a,xs)]
    -- (<*>) :: f (a -> b) -> f a -> f b
    Parser p <*> Parser q = Parser $ \xs -> do
        (f, ys) <- p xs
        (x, zs) <- q ys
        return (f x, zs)

instance Alternative Parser where
    empty = Parser $ const []
    Parser p <|> Parser q = Parser $ \cs -> p cs ++ q cs
    --Parser p <|> Parser q = Parser $ \cs -> case p cs of [] -> q cs ; x -> x

instance MonadPlus Parser where
    mzero = empty
    Parser p `mplus` Parser q = Parser $ \cs -> p cs ++ q cs
(<++>) :: MonadPlus m => m a -> m a -> m a
(<++>) = mplus

(<?>) :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m a
c <?> p = do
        x <- p
        if c x then return x else empty

chainl, chainr :: (Monad p, Alternative p) => p (a -> a -> a) -> p a -> p a
chainl op p = foldl (flip ($)) <$> p <*> many (flip <$> op <*> p)
chainr op p = do
    xs <- many $ flip ($) <$> p <*> op
    e <- p
    return $ foldr ($) e xs

--------  Parser combinators  --------

char :: Parser Word8
char = Parser $ \str -> if BS.null str then [] else [(BS.head str, BS.tail str)]

oneOf :: ByteString -> Parser Word8
oneOf xs = flip BS.elem xs <?> char

oneOfs :: [ByteString] -> Parser ByteString
oneOfs = foldl (<++>) empty . map symbol

character :: Word8 -> Parser Word8
character c = (c == ) <?> char

string :: ByteString -> Parser ByteString
string str = if str == "" then return "" else do
    x <- character $ BS.head str
    xs <- string $ BS.tail str
    return $ BS.cons x xs

spaces :: Parser ByteString
spaces = fmap BS.pack . many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do { x <- p; spaces; return x }

parens :: Parser a -> Parser a
parens m = do
  symbol "("
  n <- m
  symbol ")"
  return n

symbol :: ByteString -> Parser ByteString
symbol xs = token (string xs)

parser :: ByteString -> a -> Parser a
parser str a = string str >> return a

word :: Parser ByteString
word = token . fmap BS.pack . some . oneOf $ lowerCase `BS.append` upperCase `BS.append` "0123456789"

any :: Parser ByteString
any = fmap BS.pack $ many char

nBytes :: Int -> Parser ByteString
nBytes 0 = return BS.empty
nBytes n = liftA2 BS.cons char $ nBytes (n - 1)

lowerCase :: ByteString 
lowerCase = BS.pack $ map (toEnum . fromEnum) ['a' .. 'z']

upperCase :: ByteString
upperCase = BS.pack $ map (toEnum . fromEnum) ['A' .. 'Z']