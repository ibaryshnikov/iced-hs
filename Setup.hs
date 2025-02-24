{-# LANGUAGE OverloadedRecordDot #-}

-- NOTE: This file is executed by Cabal in order to build the Rust bridge library,
-- and make it available during the configuration of the package.
-- This is necessary because the Haskell library depends on
-- `libiced_hs.a` quite early during the build process,
-- due to the `extra-libraries` stanza.

module Main where

import Control.Monad
import Data.Maybe
import qualified Data.List
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Utils.Path
import System.Directory
import System.Environment
import System.Process


-- extra-bundled-libraries expects libraries to be of the form `libC<name>.a` and `lib<name>.so`
-- in this scenario it is unclear if the library name in extra-libraries should be `Ciced_hs` or `iced_hs`.
-- so we are generating both `libCiced_hs.a` and `libiced_hs.so`
-- if we used `iced_hs` as the extra library then, we need rpath but this rpath is a relative one and it'll
-- point to a temporary directory when iced-hs is used as a third party library, this is likely okay and
-- won't cause any issues
--
-- There are three iced-hs-<version>[-inplace].conf files we need to think about:
-- * Two in dist-newstyle, they are used for local development;
-- * One in `~/.cabal/store/` when other packages depend on us either via their build-depends or source-repository-package.
--
-- Let's first see how the .conf files in dist-newstyle affect us
-- we need a relative extra-lib-dirs in:
--     `dist-newstyle/build/x86_64-linux/ghc-9.10.1/iced-hs-0.0.3.0/package.conf.inplace/iced-hs-0.0.3.0-inplace.conf`
-- ^ this file is generated in buildHook and extra-lib-dirs is needed here because cabal won't search for
-- extra-library in the libraries own build directory, the other files are in:
-- * `dist-newstyle/packagedb/ghc-9.10.1/iced-hs-0.0.3.0-inplace.conf`;
-- * `~/.cabal/store/ghc-9.10.1[-inplace]/packagedb/iced-hs-<version>-<unitid>.conf`
-- ^ these are generated in regHook, we don't need extra-lib-dirs here because the bundled library is in
-- the same directory as the haskell library so the paths are already in search path (and added to rpath in dynamic
-- case)
--
-- Why can't we use `${pkgroot}` variable so ghc-pkg resolves it?
-- Because the structure is different in local build `${pkgroot}/../build/libiced_hs.so` vs when it's installed which
-- would be `${pkgroot}/../lib/libiced_hs.so`
--
-- NOTE: The warning from Wmissed-extra-shared-lib is most likely a ghc bug, because it tries to open `libCiced_hs.so` but
-- extra-libraries can only support .so files in the format `libiced_hs.so`, so then why does it try to open
-- `libCiced_hs.so`? unsure
-- Why don't we fix the above warning by naming the libraries same? that leads to bizarre behaviour because ghc sometimes
-- strips `Ciced_hs` to `iced_hs` and then linker looks for `iced_hs.so` which doesn't exist

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { confHook = rustConfHook
      , buildHook = \pd lbi uh bf -> do
          let library = fromJust $ PD.library pd
          let libraryBuildInfo = PD.libBuildInfo library
          let rawBuildDir = interpretSymbolicPathCWD $ buildDir lbi
          let pd' = pd { PD.library = Just $ library {
                          PD.libBuildInfo = libraryBuildInfo {
                            PD.extraLibs = "Ciced_hs" : (PD.extraLibs libraryBuildInfo),
                            PD.extraLibDirs = (makeSymbolicPath rawBuildDir) : PD.extraLibDirs libraryBuildInfo
                            ,PD.ldOptions = ("-Wl,-rpath," ++ rawBuildDir) : (PD.ldOptions libraryBuildInfo)
                            }}}
          let lbi' = lbi { localPkgDescr = pd' }
          (buildHook simpleUserHooks) pd' lbi' uh bf
      , copyHook = \pd lbi uh iff -> do
          let library = fromJust $ PD.library pd
          let libraryBuildInfo = PD.libBuildInfo library
          let pd' = pd { PD.library = Just $ library {
                          PD.libBuildInfo = libraryBuildInfo {
                            PD.extraLibs = "Ciced_hs" : (PD.extraLibs libraryBuildInfo)
                            }}}
          let lbi' = lbi { localPkgDescr = pd' }
          (copyHook simpleUserHooks) pd' lbi' uh iff
      , instHook = \pd lbi uh iff -> do
          let library = fromJust $ PD.library pd
          let libraryBuildInfo = PD.libBuildInfo library
          let pd' = pd { PD.library = Just $ library {
                           PD.libBuildInfo = libraryBuildInfo {
                             PD.extraLibs = "Ciced_hs" : (PD.extraLibs libraryBuildInfo)
                             }}}
          let lbi' = lbi { localPkgDescr = pd' }
          (instHook simpleUserHooks) pd' lbi' uh iff
      ,  regHook = \pd lbi uh rf -> do
          let library = fromJust $ PD.library pd
          let libraryBuildInfo = PD.libBuildInfo library
          let pd' = pd { PD.library = Just $ library {
                          PD.libBuildInfo = libraryBuildInfo {
                            PD.extraLibs = "Ciced_hs" : (PD.extraLibs libraryBuildInfo)
                            }}}
          let lbi' = lbi { localPkgDescr = pd' }
          (regHook simpleUserHooks) pd' lbi' uh rf
      }

rustConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
rustConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  symbolicCargoBuildDir <- getCargoBuildDir $ fromFlagOrDefault False (configProf flags)
  let rustBuildDir = getSymbolicPath symbolicCargoBuildDir
  let rawBuildDir = interpretSymbolicPathCWD $ buildDir localBuildInfo
  putStrLn "Building Rust code..."
  rawSystemExit (fromFlag $ flags.configCommonFlags.setupVerbosity) Nothing "cargo" ["build"]
  putStrLn "Build Rust code success!"
  -- It is safer to rename an archive than a .so file
  -- TODO: force it overwrite when dest exists
  rawSystemExit (fromFlag $ flags.configCommonFlags.setupVerbosity) Nothing "mv" [(rustBuildDir </> "libiced_hs.a"), (rustBuildDir </> "libCiced_hs.a")]
  copyFiles (fromFlag $ configVerbosity flags) rawBuildDir [(rustBuildDir, "libCiced_hs.a")]
  copyFiles (fromFlag $ configVerbosity flags) rawBuildDir [(rustBuildDir, "libiced_hs.so")]
  pure localBuildInfo

getCargoBuildDir :: Bool -> IO (SymbolicPath from to)
getCargoBuildDir isRelease = do
  cargoPath <- readProcess "cargo" ["locate-project", "--workspace", "--message-format=plain"] ""
  let dir = take (length cargoPath - 11) cargoPath -- <dir>/Cargo.toml -> <dir>
  let targetDir = if isRelease then "release" else "debug"
  pure $ makeSymbolicPath $ dir ++ "target/" ++ targetDir
