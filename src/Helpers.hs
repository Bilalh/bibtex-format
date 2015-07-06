{-# LANGUAGE LambdaCase #-}
module Helpers where

import Control.Applicative((<$>))
import Control.Arrow ( (&&&) )

import Data.Aeson (FromJSON(..))
import Data.Char ( toLower )
import Data.List (group, sort)

import System.Directory (doesFileExist)

import Text.BibTeX.Entry

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified System.Exit as E ( exitFailure )
import qualified System.IO as E ( hPutStrLn, stderr )
import qualified Data.Char as Char


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


urlDecode :: String -> Maybe String
urlDecode [] = Just []
urlDecode ('%':xs) =
  case xs of
    (a:b:xss) ->
      urlDecode xss
      >>= return . ((Char.chr . read $ "0x" ++ [a,b]) :)
    _ -> Nothing
urlDecode ('+':xs) = urlDecode xs >>= return . (' ' :)
urlDecode (x:xs) = urlDecode xs >>= return . (x :)
