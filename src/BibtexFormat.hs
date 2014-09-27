{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative((<$>))
import Control.Arrow ( (&&&) )
import Control.Monad ( unless )

import Data.List ( nub, sortBy)
import Data.Map(Map)
import Data.Maybe(mapMaybe,fromMaybe)
import Data.Ord ( comparing )
import Data.String.Utils ( replace )

import System.Console.CmdArgs.Implicit

import Text.BibTeX.Entry
import Text.BibTeX.Format ( entry )
import Text.BibTeX.Parse ( file )
import Text.ParserCombinators.Parsec ( parse )

import qualified Data.Map as M
import qualified Data.Set as S


import Helpers


data Args = Args{
                   keys     :: Bool
                 , toRemove :: [String]
                 , repsPath :: Maybe FilePath
                }  deriving (Show, Data, Typeable)

argsDef :: Args
argsDef  = Args
             {
               toRemove = [] &= typ "bibfield" &= help "Fields to remove using multiple -t"
             , repsPath = def &= help "Replacements filepath"
             , keys     = def &= help "Process bibtex citekeys"

             }
         &= summary (unlines
            [ "bibtex-format:"
            , "* Sorts a bibtex file"
            , "* inlines crossrefs"
            , "* Applies replacements to journal/conf name"
            , "* Removes specifed fields"
            ])
         &= helpArg [name "h"]


main :: IO ()
main = do
    flags <- cmdArgs argsDef
    repsMaybe <- getJSON (repsPath flags) :: IO (Maybe (Map String String))
    let reps = fromMaybe M.empty repsMaybe


    stdin <- getContents
    case parse file "<stdin>" (fixStdin stdin) of
        Left err -> die (show err)
        Right xs -> do
            let
                entries = xs
                    |> map lowerCaseFieldNames
                    |> map lowerCaseEntryType

                stdout = entries
                    |> map (inlineCrossRef $ M.fromList . map (identifier &&& id) $ entries)
                    |> map (doPubReplacements "booktitle" reps)
                    |> map (doPubReplacements "journal" reps)
                    |> map (removeUnwantedFields flags)
                    |> map (processCiteKeys flags)
                    |> sortBy (comparing comp)
                    |> map entry
                    |> nub
                    |> unlines

                stderr = unlines
                    $ "Error: Duplicate bibtex keys."
                    : map (\ (k,n) -> show n ++ "\t" ++ k ) (duplicates xs)

            putStrLn stdout
            unless (null (duplicates xs) ) (die stderr)


processCiteKeys :: Args -> T -> T
processCiteKeys Args{..} t@Cons{identifier=cid} | keys  =
        t{identifier= replace "DBLP:" "" cid}

processCiteKeys _ t = t


inlineCrossRef :: Map String T -> T -> T
inlineCrossRef ts t@Cons{fields=fc} |
      Just refId <- "crossref" `fieldOf` t
    , Just Cons{fields=ft} <- refId `M.lookup` ts =
        let merged = M.union (M.fromList fc) (M.fromList ft)

        in  t{ fields = M.toList . M.delete "crossref" $ merged}

inlineCrossRef _ t = t


doPubReplacements :: String -> Map String String -> T -> T
doPubReplacements kind reps t@Cons{fields=fc} |
     Just pubName <- kind `fieldOf` t
    ,Just newName  <- pubName `M.lookup` reps
    = t{fields= map (fix kind newName) fc }

    where
    fix s newName (r, _)  | r == s = (s,newName)
    fix _ _ tu = tu

doPubReplacements _ _ t = t


removeUnwantedFields :: Args -> T -> T
removeUnwantedFields Args{..} c@Cons{fields=fs,entryType=ty} =
    let
        fs' = map unwrap fs
        rmFields "misc" = toRemove ++ rm
        rmFields _      = toRemove ++ "url" :rm

        onlyWanted =  [ f | f <- fs', snd f /= "", fst f `S.notMember` (  S.fromList $  rmFields ty ) ]
        firstOnly =  mapMaybe (\a -> (\b -> (a,b)) <$>  a `M.lookup` M.fromList onlyWanted ) firstFields
        rest =  sortBy (comparing fst) [ f | f <- onlyWanted, fst f `S.notMember` S.fromList firstFields ]


    in
        c{fields=  firstOnly ++ rest  }

    where
        unwrap (k,v) = (k,  dropWS . replace "\r" "" . replace "\n" " " . dropWS $  v)
        dropWS =  unwords . words

        firstFields = [ "author", "editor", "title", "booktitle", "pages", "year" ]

        rm = [
          ""
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
         ,"biburl"
         ,"citation_identifier"
         ,"citeseer-references"
         ,"date-added"
         ,"date-modified"
         ,"descriptor"
         ,"file"
         ,"google_scholar_bibtex_export_key"
         ,"ieee_dockey"
         ,"keyword"
         ,"keywords"
         ,"medium_consulted"
         ,"microsoftid"
         ,"notes"
         ,"organization"
         ,"pii"
         ,"publicationstatus"
         ,"right"
         ,"sentelink"
         ,"subject"
         ,"tags"
         ,"timestamp"
         ,"uid"
         ,"uuid"
         ,"web_data_source"
         ]


fixStdin :: String -> String
fixStdin = replace "@inproc.{" "@inproceedings{"
         . replace "@InProc.{" "@inproceedings{"
