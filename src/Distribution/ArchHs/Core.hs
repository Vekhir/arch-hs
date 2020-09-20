{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- The core functions of @arch-hs@.
module Distribution.ArchHs.Core
  ( getDependencies,
    cabalToPkgBuild,
    evalConditionTree,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Data.List (intercalate, stripPrefix)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Local
import Distribution.ArchHs.PkgBuild
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.PackageDescription
import Distribution.Pretty (prettyShow)
import Distribution.SPDX
import Distribution.System (Arch (X86_64), OS (Linux))
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.CondTree (simplifyCondTree)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unqualComponentNameToPackageName)
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange
import Distribution.Utils.ShortText (fromShortText)

archEnv :: FlagAssignment -> ConfVar -> Either ConfVar Bool
archEnv _ (OS Linux) = Right True
archEnv _ (OS _) = Right False
archEnv _ (Arch X86_64) = Right True
archEnv _ (Arch _) = Right False
archEnv _ (Impl GHC range) = Right $ withinRange (mkVersion [8, 10, 2]) range
archEnv _ (Impl _ _) = Right False
archEnv assignment f@(Flag f') = go f $ lookupFlagAssignment f' assignment
  where
    go _ (Just r) = Right r
    go x Nothing = Left x

-- | Simplify the condition tree from 'GenericPackageDescription' with given flag assignments and archlinux system assumption.
evalConditionTree ::
  (Semigroup k, L.HasBuildInfo k, Member FlagAssignmentsEnv r) =>
  GenericPackageDescription ->
  CondTree ConfVar [Dependency] k ->
  Sem r BuildInfo
evalConditionTree cabal cond = do
  flagAssignments <- ask
  let name = getPkgName' cabal
      packageFlags = genPackageFlags cabal
      defaultFlagAssignments =
        foldr (\f acc -> insertFlagAssignment (flagName f) (flagDefault f) acc) (mkFlagAssignment []) packageFlags
      flagAssignment = case Map.lookup name flagAssignments of
        Just f -> unFlagAssignment f
        _ -> []
      flagNames = fmap fst flagAssignment
      thisFlag =
        mkFlagAssignment
          . (<> flagAssignment)
          . filter (\(fName, _) -> fName `notElem` flagNames)
          $ (unFlagAssignment defaultFlagAssignments)
  return $ (^. L.buildInfo) . snd $ simplifyCondTree (archEnv thisFlag) cond

-----------------------------------------------------------------------------

-- | Get dependencies of a package recursively.
-- All version constraints will be discarded,
-- and only packages depended by executables, libraries, and test suits will be collected.
getDependencies ::
  Members [HackageEnv, FlagAssignmentsEnv, WithMyErr, DependencyRecord] r =>
  -- | Resolved
  Set PackageName ->
  -- | Skipped
  [UnqualComponentName] ->
  -- | Target
  PackageName ->
  Sem r ((G.AdjacencyMap (Set DependencyType) PackageName), Set PackageName)
getDependencies resolved skip name = do
  cabal <- getLatestCabal name
  -- Ignore subLibraries
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (subLibDeps, subLibToolsDeps) <- collectSubLibDeps cabal skip
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal skip
  (testDeps, testToolsDeps) <- collectTestDeps cabal skip
  -- Ignore benchmarks
  -- (benchDeps, benchToolsDeps) <- collectBenchMarkDeps cabal skip
  let uname :: (UnqualComponentName -> DependencyType) -> ComponentPkgList -> [(DependencyType, PkgList)]
      uname cons list = zip (fmap (cons . fst) list) (fmap snd list)

      flatten :: [(DependencyType, PkgList)] -> [(DependencyType, PackageName)]
      flatten = mconcat . fmap (\(t, pkgs) -> zip (repeat t) pkgs)

      withThisName :: [(DependencyType, PackageName)] -> [(DependencyType, PackageName, PackageName)]
      withThisName = fmap (\(t, pkg) -> (t, name, pkg))

      ignoreSingle x = not $ x `elem` ignoreList || x `elem` resolved
      ignor = filter ignoreSingle
      ignorFlatten k = filter (\(_, x) -> ignoreSingle x) . flatten . uname k

      filteredLibDeps = ignor libDeps
      filteredLibToolsDeps = ignor libToolsDeps
      filteredExeDeps = ignorFlatten CExe $ exeDeps
      filteredExeToolsDeps = ignorFlatten CExeBuildTools $ exeToolsDeps
      filteredTestDeps = ignorFlatten CTest $ testDeps
      filteredTestToolsDeps = ignorFlatten CTest $ testToolsDeps
      filteredSubLibDeps = ignorFlatten CSubLibs $ subLibDeps
      filteredSubLibToolsDeps = ignorFlatten CSubLibsBuildTools $ subLibToolsDeps

      filteredSubLibDepsNames = fmap unqualComponentNameToPackageName . fmap fst $ subLibDeps
      ignoredSubLibs = filter (`notElem` filteredSubLibDepsNames)

      currentLib = G.edges $ zip3 (repeat $ Set.singleton CLib) (repeat name) filteredLibDeps
      currentLibDeps = G.edges $ zip3 (repeat $ Set.singleton CLibBuildTools) (repeat name) filteredLibToolsDeps

      componentialEdges =
        G.edges
          . fmap (\(x, y, z) -> (Set.singleton x, y, z))
          . withThisName

      currentSubLibs = componentialEdges filteredSubLibDeps
      currentSubLibsTools = componentialEdges filteredSubLibToolsDeps
      currentExe = componentialEdges filteredExeDeps
      currentExeTools = componentialEdges filteredExeToolsDeps
      currentTest = componentialEdges filteredTestDeps
      currentTestTools = componentialEdges filteredTestToolsDeps

      -- currentBench = componentialEdges Types.Benchmark benchDeps
      -- currentBenchTools = componentialEdges BenchmarkBuildTools benchToolsDeps

      (<+>) = G.overlay
  -- Only solve lib & exe deps recursively.
  nextLib <- mapM (getDependencies (Set.insert name resolved) skip) $ ignoredSubLibs $ filteredLibDeps
  nextExe <- mapM (getDependencies (Set.insert name resolved) skip) $ ignoredSubLibs $ fmap snd filteredExeDeps
  nextSubLibs <- mapM (getDependencies (Set.insert name resolved) skip) $ fmap snd filteredSubLibDeps
  let temp = [nextLib, nextExe, nextSubLibs]
      nexts = G.overlays $ temp ^. each ^.. each . _1
      subsubs = temp ^. each ^.. each . _2 ^. each
  return $
    ( currentLib
        <+> currentLibDeps
        <+> currentExe
        <+> currentExeTools
        <+> currentTest
        <+> currentTestTools
        <+> currentSubLibs
        <+> currentSubLibsTools
        -- <+> currentBench
        -- <+> currentBenchTools
        <+> nexts,
      Set.fromList filteredSubLibDepsNames <> subsubs
    )

collectLibDeps :: Members [FlagAssignmentsEnv, DependencyRecord] r => GenericPackageDescription -> Sem r (PkgList, PkgList)
collectLibDeps cabal = do
  case cabal & condLibrary of
    Just lib -> do
      let name = getPkgName' cabal
      info <- evalConditionTree cabal lib
      let libDeps = fmap unDepV $ buildDependsIfBuild info
          toolDeps = fmap unExeV $ buildToolDependsIfBuild info
      updateDependencyRecord name libDeps
      updateDependencyRecord name toolDeps
      return (fmap fst libDeps, fmap fst toolDeps)
    Nothing -> return ([], [])

collectComponentialDeps ::
  (Semigroup k, L.HasBuildInfo k, Members [FlagAssignmentsEnv, DependencyRecord] r) =>
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (ComponentPkgList, ComponentPkgList)
collectComponentialDeps f cabal skip = do
  let conds = cabal & f
      name = getPkgName' cabal
  info <- filter (not . (`elem` skip) . fst) . zip (conds <&> fst) <$> mapM (evalConditionTree cabal . snd) conds
  let deps = info <&> ((_2 %~) $ fmap unDepV . buildDependsIfBuild)
      toolDeps = info <&> ((_2 %~) $ fmap unExeV . buildToolDependsIfBuild)
      k = fmap (\(c, l) -> (c, fmap fst l))
  mapM_ (updateDependencyRecord name) $ fmap snd deps
  mapM_ (updateDependencyRecord name) $ fmap snd toolDeps
  return (k deps, k toolDeps)

collectExeDeps :: Members [FlagAssignmentsEnv, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
collectExeDeps = collectComponentialDeps condExecutables

collectTestDeps :: Members [FlagAssignmentsEnv, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
collectTestDeps = collectComponentialDeps condTestSuites

collectSubLibDeps :: Members [FlagAssignmentsEnv, DependencyRecord] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
collectSubLibDeps = collectComponentialDeps condSubLibraries

updateDependencyRecord :: Member DependencyRecord r => PackageName -> [(PackageName, VersionRange)] -> Sem r ()
updateDependencyRecord parent deps = modify' $ Map.insertWith (<>) parent deps

-- collectBenchMarkDeps :: Members [HackageEnv, FlagAssignmentEnv] r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (ComponentPkgList, ComponentPkgList)
-- collectBenchMarkDeps = collectComponentialDeps condBenchmarks

-----------------------------------------------------------------------------

-- | Generate 'PkgBuild' for a 'SolvedPackage'.
cabalToPkgBuild :: Members [HackageEnv, FlagAssignmentsEnv, WithMyErr] r => SolvedPackage -> PkgList -> Sem r PkgBuild
cabalToPkgBuild pkg ignored = do
  let name = pkg ^. pkgName
  cabal <- packageDescription <$> (getLatestCabal name)
  let _hkgName = pkg ^. pkgName & unPackageName
      rawName = toLower' _hkgName
      _pkgName = maybe rawName id $ stripPrefix "haskell-" rawName
      _pkgVer = prettyShow $ getPkgVersion cabal
      _pkgDesc = fromShortText $ synopsis cabal
      getL (NONE) = ""
      getL (License e) = getE e
      getE (ELicense (ELicenseId x) _) = show . mapLicense $ x
      getE (ELicense (ELicenseIdPlus x) _) = show . mapLicense $ x
      getE (ELicense (ELicenseRef x) _) = "custom:" <> licenseRef x
      getE (EAnd x y) = getE x <> " " <> getE y
      getE (EOr x y) = getE x <> " " <> getE y

      _license = getL . license $ cabal
      _enableCheck = any id $ pkg ^. pkgDeps & mapped %~ (\dep -> selectDepKind Test dep && dep ^. depName == pkg ^. pkgName)
      depends = pkg ^. pkgDeps ^.. each . filtered (\x -> notMyself x && notInGHCLib x && (selectDepKind Lib x || selectDepKind Exe x) && notIgnore x)
      makeDepends =
        pkg ^. pkgDeps
          ^.. each
            . filtered
              ( \x ->
                  x `notElem` depends
                    && notMyself x
                    && notInGHCLib x
                    && ( selectDepKind LibBuildTools x
                           || selectDepKind Test x
                           || selectDepKind TestBuildTools x
                       )
                    && notIgnore x
              )
      depsToString deps = deps <&> (wrap . fixName . unPackageName . _depName) & intercalate " "
      _depends = depsToString depends
      _makeDepends = depsToString makeDepends
      _url = getUrl cabal
      wrap s = '\'' : s <> "\'"
      notInGHCLib x = (x ^. depName) `notElem` ghcLibList
      notMyself x = x ^. depName /= name
      notIgnore x = x ^. depName `notElem` ignored
      selectDepKind k x = k `elem` (x ^. depType & mapped %~ dependencyTypeToKind)
  return PkgBuild {..}