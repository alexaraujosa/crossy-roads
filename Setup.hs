import Distribution.Verbosity
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup(BuildFlags, CleanFlags)
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Data.List
import System.Directory
import System.FilePath
import System.Process
import System.Info
import Control.Applicative
import Control.Monad

{-
=======================================
                SETTINGS               
=======================================
-}

outDir = "build"
assets = ("src/Defaults", "kfcrun")

{-
=======================================
            HOOK FUNCTIONS             
=======================================
-}

main = defaultMain--defaultMainWithHooks (simpleUserHooks { postBuild = myPostBuild })

myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild args bFLags pd lbi = do
    let bldParts = wordsWhen (=='/') (buildDir lbi)
    let bldSelParts = mapAccumL (\c p -> if elem "dist-newstyle" c then (c,p) else (c ++ [p], p)) [] bldParts
    let distPath = (if os == "mingw32" then "" else "/") ++ intercalate "/" (fst bldSelParts)

    let rootPath = (if os == "mingw32" then "" else "/") ++ intercalate "/" (init $ fst bldSelParts)
    let outPath = rootPath ++ "/" ++ outDir
    let buildPath = buildDir lbi
    let tmpPath = distPath ++ "/tmp/setup"
    let zipPath = tmpPath ++ "/zip"
    let zipExePath = zipPath ++ "/executables"

    let pkgname = unPackageName $ pkgName $ package pd

    print distPath

    createDirectoryIfMissing True tmpPath
    createDirectory zipPath
    createDirectory zipExePath

    let exes = executables pd
    mapM (\e -> buildExecutable e (rootPath,tmpPath,buildPath,zipExePath) pd lbi) exes

    readProcess (rootPath ++ "/scripts/zipLocally") [(zipPath ++ "/" ++ pkgname ++ ".zip"), zipExePath] ""

    createDirectoryIfMissing True outPath
    copyFile (zipPath ++ "/" ++ pkgname ++ ".zip") (outPath ++ "/" ++ pkgname ++ ".zip")

{-
=======================================
            HELPER FUNCTIONS           
=======================================
-}


buildExecutable :: Executable -> (String,String,String,String) -> PackageDescription -> LocalBuildInfo -> IO ()
buildExecutable exe (rootPath,tmpPath,buildPath,zipExePath) pd lbi = do
    let name = unUnqualComponentName $ exeName exe
    let tmpDirPath = tmpPath ++ "/" ++ name
    let tmpAssetPath = tmpDirPath ++ "/" ++ (snd assets)
    let assetPath = rootPath ++ "/" ++ (fst assets)
    let exePath = buildPath ++ "/" ++ name ++ "/" ++ name

    putStrLn ("Building assets for: " ++ name)
    createDirectoryIfMissing True (tmpPath ++ "/" ++ name)
    copyDirectoryRecursive silent assetPath tmpAssetPath

    qdc <- getQualifiedDirectoryContents tmpAssetPath
    removeEmptyFiles qdc
    copyFile exePath (tmpDirPath ++ "/" ++ name)

    --readProcess "zip" ["-jr", (zipExePath ++ "/" ++ name ++ ".zip"), tmpDirPath] ""
    readProcess (rootPath ++ "/scripts/zipLocally") [(zipExePath ++ "/" ++ name ++ ".zip"), tmpDirPath] ""

    putStrLn ("Successfully built assets for: " ++ name)
    return ()

removeEmptyFiles :: [String] -> IO ()
removeEmptyFiles fl@(f:tf) = do
    mapM_ (\f -> do
        qdc <- getQualifiedDirectoryContents f

        mapM (\mf -> do
                dfe <- doesFileExist mf
                if dfe then
                        when (last (wordsWhen (=='/') mf) == "_empty") $ removeFile mf--putStrLn ("DIELETED " ++ mf)
                    else removeEmptyFiles [mf]
            ) qdc 
        ) fl




wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of {
        "" -> [];
        s' -> w : wordsWhen p s'' where (w, s'') = break p s';
    }

getQualifiedDirectoryContents :: FilePath -> IO [FilePath]
getQualifiedDirectoryContents fp =
    map (fp </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents fp
