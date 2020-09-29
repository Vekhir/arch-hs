{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability : portable
-- Types used in this project.
module Distribution.ArchHs.Types
  ( PkgList,
    ComponentPkgList,
    CommunityName (..),
    CommunityVersion,
    CommunityDB,
    HackageEnv,
    CommunityEnv,
    FlagAssignmentsEnv,
    DependencyType (..),
    DependencyKind (..),
    DependencyProvider (..),
    SolvedPackage (..),
    SolvedDependency (..),
    FlagAssignments,
    depProvider,
    pkgProvider,
    pkgName,
    pkgDeps,
    depName,
    depType,
    DependencyRecord,
  )
where

import           Data.Map.Strict                      (Map)
import           Distribution.ArchHs.Internal.Prelude
import           Distribution.Hackage.DB              (HackageDB)
import           Lens.Micro.TH                        (makeLenses)


-- | A list of 'PackageName'
type PkgList = [PackageName]

-- | A list of component represented by 'UnqualComponentName' and its dependencies collected in a 'PkgList'
type ComponentPkgList = [(UnqualComponentName, PkgList)]

-- Name of packages in archlinux community repo
newtype CommunityName = CommunityName {unCommunityName :: String}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Version of packages in archlinux community repo
type CommunityVersion = String

-- | Representation of @cummunity.db@
type CommunityDB = Map CommunityName CommunityVersion

-- | Reader effect of 'HackageDB'
type HackageEnv = Reader HackageDB

-- | Reader effect of 'CommunityDB'
type CommunityEnv = Reader CommunityDB

-- | A map of packages with their 'FlagAssignment'
type FlagAssignments = Map PackageName FlagAssignment

-- | Reader effect of a map, associating 'PackageName' with its 'FlagAssignment'
type FlagAssignmentsEnv = Reader FlagAssignments

-- | Unused state effect
type DependencyRecord = State (Map PackageName [VersionRange])

-- | The type of a dependency. Who requires this?
data DependencyType
  = -- | By a /executable/
    CExe UnqualComponentName
  | -- | By the /build tools/ of a /executable/
    CExeBuildTools UnqualComponentName
  | -- | By a /library/
    CLib
  | -- | By a /test suit/
    CTest UnqualComponentName
  | -- | By a /benchmark/
    CBenchmark UnqualComponentName
  | -- | By the /build tools/ of a /library/
    CLibBuildTools
  | -- | By the /build tools/ of a /test suit/
    CTestBuildTools UnqualComponentName
  | -- | By the /build tools/ of a /benchmark/
    CBenchmarkBuildTools UnqualComponentName
  | -- |  By a /sub-library/
    CSubLibs UnqualComponentName
  | -- |  By the /build tools/ of a /sub-library/
    CSubLibsBuildTools UnqualComponentName
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Tags of data constructors of 'DependencyType'
data DependencyKind
  = Exe
  | ExeBuildTools
  | Lib
  | Test
  | Benchmark
  | LibBuildTools
  | TestBuildTools
  | BenchmarkBuildTools
  | SubLibs
  | SubLibsBuildTools
  deriving stock (Eq)

instance Show DependencyType where
  show (CExe x) = unUnqualComponentName x <> " :: Exe"
  show (CExeBuildTools x) = unUnqualComponentName x <> " :: ExeBuildTools"
  show (CTest x) = unUnqualComponentName x <> " :: Test"
  show (CBenchmark x) = unUnqualComponentName x <> " :: Benchmark"
  show (CTestBuildTools x) = unUnqualComponentName x <> " :: TestBuildTools"
  show (CBenchmarkBuildTools x) = unUnqualComponentName x <> " :: BenchmarkBuildTools"
  show (CSubLibs x) = unUnqualComponentName x <> " :: SubLibs"
  show (CSubLibsBuildTools x) = unUnqualComponentName x <> " :: SubLibsBuildTools"
  show CLib = "Lib"
  show CLibBuildTools = "LibBuildTools"

-- | Provider of a dependency.
data DependencyProvider = ByCommunity | ByAur
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show DependencyProvider where
  show ByCommunity = "community"
  show ByAur       = "aur"

-- | A solved dependency, holden by 'SolvedPackage'
data SolvedDependency = SolvedDependency
  { -- | Provider of this dependency
    _depProvider :: Maybe DependencyProvider,
    -- | Name of the dependency
    _depName     :: PackageName,
    -- | Types of the dependency
    _depType     :: [DependencyType]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | A solved package collected from dgraph. This data type is not designed to be recursively,
-- thus the element type of '_pkgDeps' is 'SolvedDependency', rather than another 'SolvedPackage'.
data SolvedPackage
  = -- | A package which has been provided by somebody, so there is no need to expand its dependencies
    ProvidedPackage
      { -- | Package name
        _pkgName     :: PackageName,
        -- | Package provider. (The name of 'DependencyProvider' may be confusing...)
        _pkgProvider :: DependencyProvider
      }
  | -- | A package with its dependencies
    SolvedPackage
      { -- | Package name
        _pkgName :: PackageName,
        -- | Package dependencies
        _pkgDeps :: [SolvedDependency]
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''SolvedDependency
makeLenses ''SolvedPackage
