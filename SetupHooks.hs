{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StaticPointers #-}
module SetupHooks where

import Debug.Trace
import Control.Monad
import Control.Monad.IO.Class
import Distribution.Simple.Program.Types (ConfiguredProgram(..))
import Distribution.Simple.SetupHooks (PreBuildComponentInputs(..) , RulesM, SetupHooks(..), BuildHooks(..), SetupHooks(..), Suffix(..), Location(..), Dict(..), Dependency(..), BuildingWhat(..))
import Distribution.Simple.SetupHooks qualified as Hooks
import Distribution.Types.BuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Utils.Path (SymbolicPath, CWD, FileOrDir(..), Pkg)
import Distribution.Utils.Path qualified as Path
import Distribution.Verbosity (Verbosity(..))
import GHC.StaticPtr
import Data.List.NonEmpty qualified as NE
import System.Directory

setupHooks :: SetupHooks
setupHooks =
 Hooks.noSetupHooks
   { buildHooks = Hooks.noBuildHooks {
      preBuildComponentRules = Just $ Hooks.rules ( static () ) preBuildRules
     }
   }

moveLibIced :: ( FilePath, FilePath) -> IO ()
moveLibIced ( libIcedArchive, destination) = do
  copyFile libIcedArchive destination

preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules pbci = do
  let verbosity = Hooks.buildingWhatVerbosity $ pbci.buildingWhat
  let sourceDir = Path.makeSymbolicPath "."
  let destDir = buildDir pbci.localBuildInfo
  let icedLibrary  = Location sourceDir ( Path.makeRelativePathEx "libiced_hs.a" )
  let destinationPath = (Path.getSymbolicPath destDir) <> "/libCiced.a"
  let destinationLocation = Location destDir (Path.makeRelativePathEx "libCiced.a")
  let command = Hooks.mkCommand (static Dict) (static moveLibIced) ("libiced_hs.a", destinationPath) 
  libIcedArchiveExists <- liftIO $ doesFileExist "libiced_hs.a"
  unless libIcedArchiveExists $
    error "libiced_hs.a does not exist"
  Hooks.addRuleMonitors [Hooks.monitorFile "libiced_hs_.a"]
  Hooks.registerRule_ "iced:library-archive" $ 
    Hooks.staticRule 
      command
      [FileDependency icedLibrary]
      (NE.singleton destinationLocation)
