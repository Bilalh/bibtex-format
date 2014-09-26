{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad ( unless )

import Data.Char(isAscii, isAlphaNum,toLower)
import Data.Map(Map)
import Data.Maybe(fromMaybe)
import Data.List ( nub, intercalate)

import System.Console.CmdArgs.Implicit

import Text.BibTeX.Entry
import Text.BibTeX.Format ( entry )
import Text.BibTeX.Parse ( file, splitAuthorList,splitSepList )
import Text.ParserCombinators.Parsec ( parse )

import qualified Data.Map as M


import Helpers


data Args = Args{
                   repsPath :: Maybe FilePath
                }  deriving (Show, Data, Typeable)

argsDef :: Args
argsDef  = Args
             {
              repsPath = def &= help "Replacements filepath"

             }
         &= summary (unlines
            [ "bibtex-new-keys:"
            , "Create consistent bibtex keys"
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
            let
                entries = xs
                    |> map lowerCaseEntryType
                    |> map (newCiteKeys reps)

                stdout = entries
                    |> map entry
                    |> nub
                    |> unlines

                stderr = unlines
                    $ "Error: Duplicate bibtex keys."
                    : map (\ (k,n) -> show n ++ "\t" ++ k ) (duplicates xs)

            putStrLn stdout
            unless (null $ duplicates xs) (die stderr)

newCiteKeys :: Map String String -> T -> T
newCiteKeys reps t@Cons{..} |
     Just (_:_:y@(_:_))   <- lookup "year" fields
    ,Just (kind, pubName) <- getKind
    ,Just a               <- getAuthors

    ,Just b  <- lookup pubName fields >>= return .  filter f . useReps
        = t{identifier=map toLower $ intercalate "/" [kind,b,y,a]}

    where
        f c =  (isAlphaNum c || c `elem` ":-/_" ) && isAscii c

        getKind
            | Just _ <- "journal"   `fieldOf` t  = Just ("j","journal")
            | Just _ <- "booktitle" `fieldOf` t  = Just ("c","booktitle")
            | otherwise                          = Nothing

        useReps s | Just newName  <- s `M.lookup` reps  = newName
        useReps s = s

        getAuthors
            | Just authors <- lookup "author" fields =
                let ls = splitAuthorList authors  in
                Just . addFstAuthor ls
                    . map ( head . last .  splitSepList ' ') . as $ ls
            | otherwise = Nothing

            where
                addFstAuthor a s =
                        let fa  = (last . splitSepList ' ' . head  $  a)
                        in fa ++  '-':s

                as []     = []
                as [_]    = []
                as (_:xs) = xs

newCiteKeys _ t = t
