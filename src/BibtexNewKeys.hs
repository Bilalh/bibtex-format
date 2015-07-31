{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, RecordWildCards #-}

module Main where

import Control.Monad                   (unless)
import Data.Char                       (isAlphaNum, isAscii, toLower,isPunctuation)
import Data.List                       (intercalate, nub)
import Data.Map                        (Map)
import Data.Maybe                      (fromMaybe)
import Helpers
import System.Console.CmdArgs.Implicit
import Text.BibTeX.Entry
import Text.BibTeX.Format              (entry)
import Text.BibTeX.Parse               (file, splitAuthorList, splitSepList)
import Text.ParserCombinators.Parsec   (parse)

import qualified Data.Map as M


data Args = Args{ repsPath :: Maybe FilePath
                } deriving (Show, Data, Typeable)

argsDef :: Args
argsDef  = Args
             { repsPath = def &= help "Replacements filepath"
             }
         &= summary (unlines
            [ "bibtex-new-keys:"
            , "Create consistent bibtex keys"
            , "e.g. bessiere:05:a_sat_based_version_space"
            ])
         &= helpArg [name "h"]


main :: IO ()
main = do
    flags <- cmdArgs argsDef
    repsMaybe <- getJSON (repsPath flags) :: IO (Maybe (Map String String))
    let reps = fromMaybe M.empty repsMaybe


    stdin <- getContents
    case parse file "<stdin>" stdin of
        Left err -> die (show err)
        Right xs -> do
            let stdout = xs
                    |> map (newCiteKeys reps)
                    |> map entry
                    |> nub
                    |> unlines

                stderr = unlines
                    $ "Error: Duplicate bibtex keys."
                    : map (\ (k,n) -> show n ++ "\t" ++ k ) (duplicates xs)

            putStrLn stdout
            unless (null $ duplicates xs) (die stderr)

-- format authors:2 digit year:words
-- e.g bessiere:05:a_sat_based_version_space
newCiteKeys :: Map String String -> T -> T
newCiteKeys _ t@Cons{..} |
     Just (_:_:y@(_:_))   <- lookup "year" fields
    ,Just a               <- getAuthors
    ,Just ti              <- getTitle
        = t{identifier= filter filterChar . map repChar $ a ++ ':' : (map toLower $ intercalate ":"  [y, ti ]) }

    where

        repChar c | c `elem` "~"  = '_'
        repChar c = c

        mapChar c | c `elem` "-~"  = ' '
        mapChar c = c

        filterChar c =  (isAlphaNum c || c `elem` ":/_" ) && isAscii c

        getAuthors
            | Just authors <- fmap (map toLower) $ lookup "author" fields =
                let ls = splitAuthorList authors  in
                    Just (head . splitSepList ' ' . head  $ ls )
            | otherwise = Nothing

        getTitle
            | Just parts <- fmap (words . filter (not . isPunctuation)
                                        . filter (isAscii) . map mapChar)
                                 $ lookup "title" fields  =
                Just . intercalate "_" $ takep 25 parts

            | otherwise = Nothing

        takep :: Int -> [String] -> [String]
        takep _    [] = []
        takep left _      | left <= 0       = []
        takep left (x:_)  | left < length x = []
        takep left (x:xs) = x : takep (left - length x - 1) xs

newCiteKeys _ t = t
