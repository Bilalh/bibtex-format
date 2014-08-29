import System.Directory ( copyFile )
import System.FilePath ( (</>) )

import Distribution.PackageDescription ( PackageDescription(..) )
import Distribution.Simple ( UserHooks(..), defaultMainWithHooks, simpleUserHooks )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), absoluteInstallDirs, bindir )
import Distribution.Simple.Setup ( fromFlag, copyDest, CopyDest(..) )


main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
        { postCopy = \ _ flags pkg lbi -> copyScripts pkg lbi (fromFlag $ copyDest flags)
        , postInst = \ _ _     pkg lbi -> copyScripts pkg lbi NoCopyDest
        }

copyScripts :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
copyScripts pkg local copy = do
    let dirs = absoluteInstallDirs pkg local copy
    copyFile "scripts/align_equals.rb"           (bindir dirs </> "align_equals.rb")
    copyFile "scripts/bibtex-fix-aligned.sh"     (bindir dirs </> "bibtex-fix-aligned")
    copyFile "scripts/sente_pick_to_bib.sh"      (bindir dirs </> "sente_pick_to_bib")
    copyFile "scripts/sente_selected_to_bib.sh"  (bindir dirs </> "sente_selected_to_bib")
    copyFile "scripts/sente_cite.sh"             (bindir dirs </> "sente_cite")

