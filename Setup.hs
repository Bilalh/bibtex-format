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
    copyFile "scripts/bibtex-format-aligned.sh"     (bindir dirs </> "bibtex-format-aligned")

