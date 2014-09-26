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


getJSON :: FromJSON a => Maybe FilePath -> IO (Maybe a)
getJSON (Just fp) = do
    b <- doesFileExist fp
    if b then
        A.decode <$> B.readFile fp
    else
        return Nothing
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
