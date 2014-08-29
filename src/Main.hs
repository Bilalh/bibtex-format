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
                stdout = xs
                    |> map lowerCaseFieldNames
                    |> map lowerCaseEntryType
                    |> (reverse .  sortBy (comparing comp))
                    |> map removeUnwantedFields
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

removeUnwantedFields :: T -> T
removeUnwantedFields c@Cons{fields=fs,entryType=typ} =
    let
        rmFields "misc" = "url" :rm
        rmFields _      = "url" :rm

        onlyWanted =  [ f | f <- fs, (snd f) /= "", (fst f) `S.notMember` (  S.fromList $ rmFields typ) ]
        firstOnly =  mapMaybe (\a -> (\b -> (a,b)) <$>  a `M.lookup` (M.fromList onlyWanted) ) firstFields
        rest =  sortBy (comparing fst) [ f | f <- onlyWanted, (fst f) `S.notMember` (S.fromList firstFields) ]


    in
        c{fields=  firstOnly ++ rest  }

    where

        firstFields = [ "author", "editor", "title", "booktitle", "pages", "year" ]

        rm = [
          ""
         ,"abstract"
         ,"accessdate"
         ,"acmid"
         ,"all_isbns"
         ,"citation_identifier"
         ,"google_scholar_bibtex_export_key"
         ,"keywords"
         ,"medium_consulted"
         ,"notes"
         ,"pii"
         ,"publicationstatus"
         ,"sentelink"
         ,"tags"
         ,"uuid"
         ,"web_data_source"
         ,"series"  -- not sure
         ]

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

histogram :: Ord a => [a] -> [(a,Int)]
histogram = map (head &&& length) . group . sort

comp :: T -> (Maybe String, String, Maybe String, String)
comp x = (fieldOf "year" x, entryType x, fieldOf "title" x, identifier x)

fieldOf :: String -> T -> Maybe String
fieldOf f = lookup f . fields

lowerCaseEntryType :: T -> T
lowerCaseEntryType t = t { entryType = map toLower (entryType t) }

fixStdin :: String -> String
fixStdin = replace "@inproc.{" "@inproceedings{"
         . replace "@InProc.{" "@inproceedings{"

die :: String -> IO ()
die err = E.hPutStrLn E.stderr err >> E.exitFailure

