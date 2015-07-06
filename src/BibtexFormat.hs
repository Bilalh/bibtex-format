{-# LANGUAGE DeriveDataTypeable, LambdaCase, OverloadedStrings, RecordWildCards,
             ScopedTypeVariables, ViewPatterns #-}
module Main where

import Helpers
import Prelude
import Strings (replace)

import Control.Applicative             ((<$>))
import Control.Arrow                   ((&&&))
import Control.Monad                   (unless)
import Data.Char                       (toLower)
import Data.List                       (isInfixOf, nub, sortBy, stripPrefix)
import Data.Map                        (Map)
import Data.Maybe                      (fromMaybe, mapMaybe)
import Data.Ord                        (comparing)
import System.Console.CmdArgs.Implicit
import Text.BibTeX.Entry
import Text.BibTeX.Format              (entry)
import Text.BibTeX.Parse               (file)
import Text.ParserCombinators.Parsec   (parse)

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Text as T


data Args = Args
    { keys     :: Bool
    , toRemove :: [String]
    , repsPath :: Maybe FilePath
    , pdfLinks :: Bool
    , extra_keys :: [String]
    , extra_vals :: [String]
    }  deriving (Show, Data, Typeable)

argsDef :: Args
argsDef  = Args
           { toRemove = [] &= typ "bibfield" &= help "Fields to remove using multiple -t"
           , repsPath = def &= help "Replacements filepath"
           , keys     = def &= help "Process bibtex citekeys" &= name "n" &= explicit
           , pdfLinks = def &= help "Convert a file:// link to a posix path and places it in a pdf field"
           , extra_keys = def &= name "k" &= help "Extra *keys* value pairs"  &= explicit
           , extra_vals = def &= name "v" &= help "Extra keys *value* pairs"  &= explicit
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
  repsMaybe :: Maybe (Map String String) <- getJSON  $ (repsPath flags)
  let reps = fromMaybe M.empty repsMaybe

  let extra_fields = zip (extra_keys flags) (extra_vals flags)

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
                  |> (\z -> if pdfLinks flags then map doPDFLinks z else z)
                  |> map (removeUnwantedFields flags)
                  |> map (processMonth)
                  |> map (processDOI)
                  |> map (addExtraFields extra_fields)
                  |> sortBy (comparing comp)
                  |> map entry
                  |> nub
                  |> unlines

              stderr = unlines
                  $ "Error: Duplicate bibtex keys."
                  : map (\ (k,n) -> show n ++ "\t" ++ k ) (duplicates xs)

          putStrLn stdout
          unless (null (duplicates xs) ) (die stderr)


doPDFLinks :: T -> T
doPDFLinks t@Cons{fields=fs} = t{fields=map process fs}
  where
    process :: (String, String) -> (String, String)
    process (_, stripPrefix "file://" -> Just rest)
        | Just decoded <- urlDecode rest
        , ".pdf" `isInfixOf` decoded =
        ("pdf", T.unpack . (T.replace ",Sente," "") . T.pack $ decoded)
    process f = f


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

processDOI :: T -> T
processDOI t@Cons{fields=fs} = t{fields=map process fs}

 where
 process ("doi", val) = ("doi", T.unpack . (T.replace "\\_" "_") . T.pack $ val)
 process tu           = tu

processMonth :: T -> T
processMonth t@Cons{fields=fs} = t{fields=map process fs}

 where
 process ("month", val) = ("month", fix $ map toLower val)
 process tu             = tu

 fix "1"  = "jan"
 fix "2"  = "feb"
 fix "3"  = "mar"
 fix "4"  = "apr"
 fix "5"  = "may"
 fix "6"  = "jun"
 fix "7"  = "jul"
 fix "8"  = "aug"
 fix "9"  = "sep"
 fix "10" = "oct"
 fix "11" = "nov"
 fix "12" = "dec"

 fix "jan" = "jan"
 fix "feb" = "feb"
 fix "mar" = "mar"
 fix "apr" = "apr"
 fix "may" = "may"
 fix "jun" = "jun"
 fix "jul" = "jul"
 fix "aug" = "aug"
 fix "sep" = "sep"
 fix "oct" = "oct"
 fix "nov" = "nov"
 fix "dec" = "dec"

 fix "january"   = "jan"
 fix "february"  = "feb"
 fix "march"     = "mar"
 fix "april"     = "apr"
 fix "june"      = "jun"
 fix "july"      = "jul"
 fix "august"    = "aug"
 fix "september" = "sep"
 fix "october"   = "oct"
 fix "november"  = "nov"
 fix "december"  = "dec"

 fix xs = error . show $ "Not a month: "  ++ xs

addExtraFields :: [(String,String)] -> T -> T
addExtraFields extra t  = t{fields=fields t ++ extra}

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
