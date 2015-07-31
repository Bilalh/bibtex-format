{-# LANGUAGE DeriveDataTypeable, LambdaCase, OverloadedStrings, RecordWildCards,
             ScopedTypeVariables, ViewPatterns, NoImplicitPrelude #-}
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
    { toRemove :: [String]
    , repsPath :: Maybe FilePath
    , pdfLinks :: Bool
    , extra_keys :: [String]
    , extra_vals :: [String]
    }  deriving (Show, Data, Typeable)

argsDef :: Args
argsDef  = Args
           { toRemove   = []  &= typ "bibfield" &= help "Fields to remove using, multiple -t allowed"
           , repsPath   = def &= help "booktitle and journal replacements in a json file e.g. {\"Commun. ACM\": \"Communications of the ACM\"}  "
           , pdfLinks   = def &= help "Convert a file:// link to a posix path and places it in the pdf field"
           , extra_keys = def &= name "k" &= help "Extra *keys* value pairs for each entry"  &= explicit
           , extra_vals = def &= name "v" &= help "Extra keys *value* pairs for each entry"  &= explicit
           }
         &= summary (unlines
            [ "bibtex-format:"
            , "* Sorts a bibtex file"
            , "* inlines crossrefs"
            , "* Applies replacements to journal/booktitle"
            , "* Removes specifed fields"
            , "* Fixes Mendeley Output"
            ])
         &= helpArg [name "h"]


main :: IO ()
main = do
  flags <- cmdArgs argsDef
  repsMaybe :: Maybe (Map String String) <- getJSON  $ (repsPath flags)
  let reps = fromMaybe M.empty repsMaybe

  let extra_fields = zip (extra_keys flags) (extra_vals flags)

  stdin <- getContents
  case parse file "<stdin>" (stdin) of
      Left err -> die (show err)
      Right xs -> do
          let
              entries = xs
                  |> map lowerCaseFieldNames
                  |> map lowerCaseEntryType
                  |> map fixEntryType

                  |> map (inlineCrossRef $ M.fromList . map (identifier &&& id) $ entries)
                  |> map (doPubReplacements "booktitle" reps)
                  |> map (doPubReplacements "journal" reps)
                  |> (\z -> if pdfLinks flags then map doPDFLinks z else z)

                  |> map (removeUnwantedFields flags)
                  |> map (removeWebJuck)

                  |> map (processMonth)
                  |> map (processDOI)
                  |> map (processAuthor)
                  |> map (protectUpper)

                  |> map (addExtraFields extra_fields)
                  |> map (sortFields)
                  |> sortBy (comparing bibComp)

              stdout = entries
                  |> map entry
                  |> nub
                  |> unlines

              stderr = unlines
                  $ "Error: Duplicate bibtex keys."
                  : map (\ (k,n) -> show n ++ "\t" ++ k ) (duplicates xs)

          putStrLn stdout
          unless (null $ duplicates xs) (die stderr)



bibComp :: T -> (Maybe Int, String, Maybe String, String)
bibComp x = (  fieldOf "year" x  >>= fmap negate . parseInt, entryType x, fieldOf "title" x, identifier x)


lowerCaseEntryType :: T -> T
lowerCaseEntryType t = t { entryType = map toLower (entryType t) }

fixEntryType :: T -> T
fixEntryType t@Cons{entryType=et} = t { entryType = f et }

  where
    f "inproc." = "inproceedings"
    f x         = x


doPDFLinks :: T -> T
doPDFLinks t@Cons{fields=fs} = t{fields=map process fs}
  where
    process :: (String, String) -> (String, String)
    process (_, stripPrefix "file://" -> Just rest)
        | decoded <- urlDecode rest
        , ".pdf" `isInfixOf` decoded =
        ("pdf", T.unpack . (\x -> (flip T.append) ".pdf" $ T.splitOn ".pdf" x !! 0  ) . T.pack $ decoded)
    process f = f


doPubReplacements :: String -> Map String String -> T -> T
doPubReplacements kind reps t@Cons{fields=fc} |
   Just pubName <- kind `fieldOf` t
  ,Just newName  <- pubName `M.lookup` reps
  = t{fields= map (fix kind newName) fc }

  where
  fix s newName (r, _)  | r == s = (s,newName)
  fix _ _ tu = tu

doPubReplacements _ _ t = t


inlineCrossRef :: Map String T -> T -> T
inlineCrossRef ts t@Cons{fields=fc} |
    Just refId <- "crossref" `fieldOf` t
  , Just Cons{fields=ft} <- refId `M.lookup` ts =
      let merged = M.union (M.fromList fc) (M.fromList ft)

      in  t{ fields = M.toList . M.delete "crossref" $ merged}

inlineCrossRef _ t = t


-- To fix mendeley'x broken doi output
processDOI :: T -> T
processDOI t@Cons{fields=fs} = t{fields=map process fs}

 where
 process ("doi", val) = ("doi", T.unpack . (T.replace "\\_" "_") . T.pack $ val)
 process tu           = tu


-- To fix mendeley'x broken Author output
processAuthor :: T -> T
processAuthor t@Cons{fields=fs} = t{fields=map process fs}

 where
 process ("author", val) = ("author", map f val)
 process tu           = tu

 f '’' =  '\''
 f x  = x

-- e.g. organization Citeseer
removeWebJuck :: T -> T
removeWebJuck t@Cons{fields=fs} = t{fields=mapMaybe process fs}

 where
 process ("organization","Citeseer") = Nothing
 process ("publisher","Citeseer")    = Nothing
 process (_,"")                      = Nothing  -- No value
 process tu                          = Just tu


protectUpper :: T -> T
protectUpper t@Cons{fields=fs} = t{fields=map process fs}

  where
  process (x,str) | x `elem` ["booktitle", "title", "journal", "Series"] = (x, protect str )
  process tu = tu

  protect = unwords . map protecter . words
  --TODO finish
  protecter s = s



addExtraFields :: [(String,String)] -> T -> T
addExtraFields extra t  = t{fields= M.toList added}
  where added = (M.fromList extra) `M.union` (M.fromList $ fields t)


sortFields :: T -> T
sortFields c@Cons{fields=fs} =
  let
      firstOnly =  mapMaybe (\a -> (\b -> (a,b)) <$>  a `M.lookup` M.fromList fs ) firstFields
      rest =  sortBy (comparing fst) [ f | f <- fs, fst f `S.notMember` S.fromList firstFields ]

  in
      c{fields=  firstOnly ++ rest  }

  where
      firstFields = [ "author", "title", "year", "month", "booktitle", "journal", "pages"]




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


removeUnwantedFields :: Args -> T -> T
removeUnwantedFields Args{..} c@Cons{fields=fs,entryType=ty} =
  let
      fs' = map unwrap fs
      rmFields "misc" = toRemove ++ rm
      rmFields _      = toRemove ++ rm

      onlyWanted = [ f | f <- fs', snd f /= "", fst f `S.notMember` (  S.fromList $  rmFields ty ) ]
  in
      c{fields=onlyWanted }

  where
      unwrap (k,v) = (k,  dropWS . replace "\r" "" . replace "\n" " " . dropWS $  v)
      dropWS =  unwords . words

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
