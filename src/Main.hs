module Main where

import Control.Applicative((<$>))
import Control.Arrow ( (&&&) )
import Control.Monad ( unless )

import Data.Char ( toLower )
import Data.List ( nub, sort, sortBy, group )
import Data.Maybe(mapMaybe)
import Data.Ord ( comparing )
import Data.String.Utils ( replace )

import Text.BibTeX.Entry
import Text.BibTeX.Format ( entry )
import Text.BibTeX.Parse ( file )
import Text.ParserCombinators.Parsec ( parse )

import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.Exit as E ( exitFailure )
import qualified System.IO as E ( hPutStrLn, stderr )

main :: IO ()
main = do
    stdin <- getContents
    case parse file "<stdin>" (fixStdin stdin) of
        Left err -> die (show err)
        Right xs -> do
            let
                entries = xs
                    |> map lowerCaseFieldNames
                    |> map lowerCaseEntryType

                stdout = entries
                    |> map (inlineCrossRef $ M.fromList . map (identifier &&& id )  $ entries)
                    |> map removeUnwantedFields
                    |> sortBy (comparing comp)
                    |> map entry
                    |> nub
                    |> unlines

                duplicates = xs
                    |> map identifier
                    |> histogram
                    |> filter (\ (_,n) -> n > 1 )

                stderr = unlines
                    $ "Error: Duplicate bibtex keys."
                    : map (\ (k,n) -> show n ++ "\t" ++ k ) duplicates

            putStrLn stdout
            unless (null duplicates) (die stderr)

inlineCrossRef :: Map String T -> T -> T
inlineCrossRef ts t@Cons{fields=fc,identifier=cid} |
      Just refId <- "crossref" `fieldOf` t
    , Just Cons{fields=ft} <- refId `M.lookup` ts =
        let merged = M.union (M.fromList fc) (M.fromList ft)
            -- newId  = replace "DBLP:" "" cid
            newId = cid
        in  t{ fields = M.toList . M.delete "crossref" $ merged
             , identifier=newId}


inlineCrossRef _ t = t


removeUnwantedFields :: T -> T
removeUnwantedFields c@Cons{fields=fs,entryType=typ} =
    let
        fs' = map unwrap fs
        rmFields "misc" = "url" :rm
        rmFields _      = "url" :rm

        onlyWanted =  [ f | f <- fs', (snd f) /= "", (fst f) `S.notMember` (  S.fromList $ rmFields typ) ]
        firstOnly =  mapMaybe (\a -> (\b -> (a,b)) <$>  a `M.lookup` (M.fromList onlyWanted) ) firstFields
        rest =  sortBy (comparing fst) [ f | f <- onlyWanted, (fst f) `S.notMember` (S.fromList firstFields) ]


    in
        c{fields=  firstOnly ++ rest  }

    where
        unwrap (k,v) = (k,  replace "\r" "" . replace "\n" " " . dropWS $  v)
        dropWS =  unwords . words

        firstFields = [ "author", "editor", "title", "booktitle", "pages", "year" ]

        rm = [
          ""
         ,"abstract"
         ,"accessdate"
         ,"acknowledgement"
         ,"acmid"
         ,"all_isbns"
         ,"annote"
         ,"bdsk-url-1"
         ,"bdsk-url-2"
         ,"bdsk-url-3"
         ,"bibdate"
         ,"bibsource"
         ,"citation_identifier"
         ,"citeseer-references"
         ,"date-added"
         ,"date-modified"
         ,"google_scholar_bibtex_export_key"
         ,"keyword"
         ,"keywords"
         ,"medium_consulted"
         ,"notes"
         ,"pii"
         ,"publicationstatus"
         ,"right"
         ,"sentelink"
         ,"subject"
         ,"tags"
         ,"uuid"
         ,"web_data_source"
         ]

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

histogram :: Ord a => [a] -> [(a,Int)]
histogram = map (head &&& length) . group . sort

comp :: T -> (Maybe Int, String, Maybe String, String)
comp x = (  fieldOf "year" x  >>= fmap negate . parseInt, entryType x, fieldOf "title" x, identifier x)


fieldOf :: String -> T -> Maybe String
fieldOf f = lookup f . fields

lowerCaseEntryType :: T -> T
lowerCaseEntryType t = t { entryType = map toLower (entryType t) }

fixStdin :: String -> String
fixStdin = replace "@inproc.{" "@inproceedings{"
         . replace "@InProc.{" "@inproceedings{"

die :: String -> IO ()
die err = E.hPutStrLn E.stderr err >> E.exitFailure

parseInt :: String -> Maybe Int
parseInt s  =  case reads s :: [(Int,String)] of
            [(d,"")] -> Just d
            _        -> Nothing
