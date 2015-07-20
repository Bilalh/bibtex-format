{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, RecordWildCards #-}

module Main where

import Control.Monad                   (unless)
import Data.Char                       (isAlphaNum, isAscii, toLower)
import Data.List                       (intercalate, nub)
import Data.Map                        (Map)
import Data.Maybe                      (fromMaybe)
import Helpers
import Strings                         (replace)
import System.Console.CmdArgs.Implicit
import Text.BibTeX.Entry
import Text.BibTeX.Format              (entry)
import Text.BibTeX.Parse               (file, splitAuthorList, splitSepList)
import Text.ParserCombinators.Parsec   (parse)

import qualified Data.Map as M


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
newCiteKeys _ t@Cons{..} |
     Just (_:_:y@(_:_))   <- lookup "year" fields
    ,Just a               <- getAuthors
    ,Just ti              <- getTitle
        = t{identifier= filter f $ a ++ ':' : (map toLower $ intercalate ":"  [y, ti ]) }

    where

        f c =  (isAlphaNum c || c `elem` ":/_" ) && isAscii c

        getAuthors
            | Just authors <- fmap (map toLower) $ lookup "author" fields =
                let ls = splitAuthorList authors  in
                    Just (last . splitSepList ' ' . head  $ ls )
            | otherwise = Nothing

        getTitle
            | Just parts <- fmap ( words . filter
                                    (\c ->  c `elem` " _" || isAscii c && isAlphaNum c) .
                                    replace "-" " "
                                )
                                $ lookup "title" fields=
                Just . intercalate "_" $ takep 25 parts

            | otherwise = Nothing

        takep :: Int -> [String] -> [String]
        takep _    [] = []
        takep left _      | left <= 0       = []
        takep left (x:_)  | left < length x = []
        takep left (x:xs) = x : takep (left - length x - 1) xs

newCiteKeys _ t = t


newCiteKeys1 :: Map String String -> T -> T
newCiteKeys1 reps t@Cons{..} |
     Just (_:_:y@(_:_))   <- lookup "year" fields
    ,Just (kind, pubName) <- getKind
    ,Just a               <- getAuthors

    ,Just b  <- lookup pubName fields >>= return . useReps
        = t{identifier=map toLower . filter f $ intercalate "/" [kind,ts b,y,a]}

    where
        ts s | length s >20 = take 20 s
        ts s = s

        f c =  (isAlphaNum c || c `elem` ":/_" ) && isAscii c

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
                        in fa ++  '_':s

                as []     = []
                as [_]    = []
                as (_:xs) = xs

newCiteKeys1 _ t = t
