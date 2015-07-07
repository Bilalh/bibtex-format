{-# LANGUAGE LambdaCase #-}
module Helpers where

import Control.Applicative ((<$>))
import Control.Arrow       ((&&&))

import Data.Aeson (FromJSON (..))
import Data.Char  (toLower)
import Data.List  (group, sort)

import System.Directory (doesFileExist)

import Text.BibTeX.Entry

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Char (chr)
import Data.Word (Word8)

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Char            as Char
import qualified System.Exit          as E (exitFailure)
import qualified System.IO            as E (hPutStrLn, stderr)



getJSON2 :: FromJSON a => FilePath -> IO a
getJSON2 = error "d"

getJSON :: FromJSON a => Maybe FilePath -> IO (Maybe a)
getJSON (Just fp) = do
  doesFileExist fp >>= \case
    False -> error $ "File not found " ++ fp
    True  -> do
      A.decode <$> B.readFile fp >>= \case
        Just v  -> return v
        Nothing -> error $ "Error decoding " ++ fp

getJSON Nothing = return Nothing

duplicates :: [T] -> [(String,Int)]
duplicates xs = xs
    |>  map identifier
    |> histogram
    |> filter (\ (_,n) -> n > 1 )


lowerCaseEntryType :: T -> T
lowerCaseEntryType t = t { entryType = map toLower (entryType t) }


(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


comp :: T -> (Maybe Int, String, Maybe String, String)
comp x = (  fieldOf "year" x  >>= fmap negate . parseInt, entryType x, fieldOf "title" x, identifier x)


fieldOf :: String -> T -> Maybe String
fieldOf f = lookup f . fields


histogram :: Ord a => [a] -> [(a,Int)]
histogram = map (head &&& length) . group . sort


die :: String -> IO ()
die err = E.hPutStrLn E.stderr err >> E.exitFailure

parseInt :: String -> Maybe Int
parseInt s  =  case reads s :: [(Int,String)] of
            [(d,"")] -> Just d
            _        -> Nothing



replacement_character :: Char
replacement_character = '\xfffd'

decode :: [Word8] -> String
decode [    ] = ""
decode (c:cs)
  | c < 0x80  = chr (fromEnum c) : decode cs
  | c < 0xc0  = replacement_character : decode cs
  | c < 0xe0  = multi1
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacement_character : decode cs
  where
    multi1 = case cs of
      c1 : ds | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
        in if d >= 0x000080 then toEnum d : decode ds
                            else replacement_character : decode ds
      _ -> replacement_character : decode cs

    multi_byte :: Int -> Word8 -> Int -> [Char]
    multi_byte i mask overlong = aux i cs (fromEnum (c .&. mask))
      where
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decode rs
          | otherwise = replacement_character : decode rs

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ rs     _ = replacement_character : decode rs


urlDecode :: String -> String
urlDecode = go []
  where
    go bs ('%':a:b:rest)           = go (fromIntegral (16 * Char.digitToInt a + Char.digitToInt b) : bs) rest
    go bs (h:t) | fromEnum h < 256 = go (fromIntegral (fromEnum h) : bs) t -- Treat ASCII as just another byte of UTF-8
    go [] []                       = []
    go [] (h:t)                    = h : go [] t -- h >= 256, so can't be part of any UTF-8 byte sequence
    go bs rest                     = decode (reverse bs) ++ go [] rest
