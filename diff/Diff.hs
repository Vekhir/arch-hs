{-# LANGUAGE OverloadedStrings #-}

module Diff
  ( diffCabal,
    Options (..),
    runArgsParser,
  )
where

import qualified Colourista as C
import Core
import Data.List (intercalate, nub, (\\))
import qualified Data.Text as T
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import qualified Distribution.Types.BuildInfo.Lens as L
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency (ExeDependency (ExeDependency))
import qualified Distribution.Types.PackageId as I
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version
import Lens.Micro
import Local (ghcLibList, ignoreList)
import Network.HTTP.Req hiding (header)
import Options.Applicative
import Polysemy
import Polysemy.Req
import Types
import Utils

data Options = Options
  { optHackagePath :: FilePath,
    optPackageName :: PackageName,
    optVersionA :: Version,
    optVersionB :: Version
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> strOption
      ( long "hackage"
          <> metavar "PATH"
          <> short 'h'
          <> help "Path to 00-index.tar"
          <> showDefault
          <> value "~/.cabal/packages/YOUR_HACKAGE_MIRROR/00-index.tar"
      )
    <*> argument optPackageNameReader (metavar "TARGET")
    <*> argument optVersionReader (metavar "VERSION_A")
    <*> argument optVersionReader (metavar "VERSION_B")

optVersionReader :: ReadM Version
optVersionReader =
  eitherReader
    ( \s -> case simpleParsec s of
        Just v -> Right v
        _ -> Left $ "Failed to parse version: " <> s
    )

optPackageNameReader :: ReadM PackageName
optPackageNameReader = eitherReader $ Right . mkPackageName

runArgsParser :: IO Options
runArgsParser =
  execParser $
    info
      (cmdOptions <**> helper)
      ( fullDesc
          <> progDesc "Try to reach the TARGET QAQ."
          <> header "arch-hs-diff - a program creating diff between different versions of a cabal file."
      )

-----------------------------------------------------------------------------

-- This parts are duplicated from Core.hs with modifications.

type VersionedList = [(PackageName, VersionRange)]

type VersionedComponentList = [(UnqualComponentName, VersionedList)]

unExe' :: ExeDependency -> (PackageName, VersionRange)
unExe' (ExeDependency name _ v) = (name, v)

collectLibDeps :: Member FlagAssignmentEnv r => GenericPackageDescription -> Sem r (VersionedList, VersionedList)
collectLibDeps cabal = do
  case cabal & condLibrary of
    Just lib -> do
      bInfo <- evalConditionTree (getPkgName cabal) lib
      let libDeps = fmap (\x -> (depPkgName x, depVerRange x)) $ targetBuildDepends bInfo
          toolDeps = fmap unExe' $ buildToolDepends bInfo
      return (libDeps, toolDeps)
    Nothing -> return ([], [])

collectRunnableDeps ::
  (Semigroup k, L.HasBuildInfo k, Member FlagAssignmentEnv r) =>
  (GenericPackageDescription -> [(UnqualComponentName, CondTree ConfVar [Dependency] k)]) ->
  GenericPackageDescription ->
  [UnqualComponentName] ->
  Sem r (VersionedComponentList, VersionedComponentList)
collectRunnableDeps f cabal skip = do
  let exes = cabal & f
  bInfo <- filter (not . (`elem` skip) . fst) . zip (exes <&> fst) <$> mapM (evalConditionTree (getPkgName cabal) . snd) exes
  let runnableDeps = bInfo <&> ((_2 %~) $ fmap (\x -> (depPkgName x, depVerRange x)) . targetBuildDepends)
      toolDeps = bInfo <&> ((_2 %~) $ fmap unExe' . buildToolDepends)
  return (runnableDeps, toolDeps)

collectExeDeps :: Member FlagAssignmentEnv r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectExeDeps = collectRunnableDeps condExecutables

collectTestDeps :: Member FlagAssignmentEnv r => GenericPackageDescription -> [UnqualComponentName] -> Sem r (VersionedComponentList, VersionedComponentList)
collectTestDeps = collectRunnableDeps condTestSuites

getCabalFromHackage :: Member (Embed IO) r => PackageName -> Version -> Bool -> Sem r GenericPackageDescription
getCabalFromHackage name version revision0 = do
  let urlPath = T.pack $ unPackageName name <> "-" <> prettyShow version
      revision0Api = https "hackage.haskell.org" /: "package" /: urlPath /: "revision" /: "0.cabal"
      normalApi = https "hackage.haskell.org" /: "package" /: urlPath /: T.pack (unPackageName name) <> ".cabal"
      api = if revision0 then revision0Api else normalApi
      r = req GET api NoReqBody bsResponse mempty
  embed $ C.infoMessage $ "Downloading cabal file from " <> renderUrl api <> "..."
  response <- reqToIO r
  case parseGenericPackageDescriptionMaybe $ responseBody response of
    Just x -> return x
    _ -> embed @IO $ fail $ "Failed to parse .cabal file from " <> show api

-----------------------------------------------------------------------------

diffCabal :: Members [FlagAssignmentEnv, WithMyErr, Embed IO] r => PackageName -> Version -> Version -> Sem r String
diffCabal name a b = do
  ga <- getCabalFromHackage name a True
  gb <- getCabalFromHackage name b False
  let pa = packageDescription ga
      pb = packageDescription gb
  (ba, ma) <- directDependencies ga
  (bb, mb) <- directDependencies gb
  return $
    unlines
      [ "Package: " <> unPackageName name,
        ver pa pb,
        desc pa pb,
        dep "Depends: \n" ba bb,
        dep "MakeDepends: \n    " ma mb
      ]

directDependencies ::
  Members [FlagAssignmentEnv, WithMyErr] r =>
  GenericPackageDescription ->
  Sem r ([String], [String])
directDependencies cabal = do
  (libDeps, libToolsDeps) <- collectLibDeps cabal
  (exeDeps, exeToolsDeps) <- collectExeDeps cabal []
  (testDeps, testToolsDeps) <- collectTestDeps cabal []
  let connectVersionWithName (n, range) = unPackageName n <> "  " <> prettyShow range
      flatten = fmap connectVersionWithName . mconcat . fmap snd
      l = fmap connectVersionWithName libDeps
      lt = fmap connectVersionWithName libToolsDeps
      e = flatten exeDeps
      et = flatten exeToolsDeps
      t = flatten testDeps
      tt = flatten testToolsDeps
      name = unPackageName $ getPkgName cabal
      notInGHCLib = (`notElem` ghcLibList) . mkPackageName
      notInIgnore = (`notElem` ignoreList) . mkPackageName
      notMyself = (/= name)
      distinct =
        filter notInIgnore
          . filter notInGHCLib
          . filter notMyself
          . nub
      depends = distinct $ l ++ e
      makedepends = (distinct $ lt ++ et ++ t ++ tt) \\ depends
  return (depends, makedepends)

diffTerm :: String -> (a -> String) -> a -> a -> String
diffTerm s f a b =
  let (ra, rb) = (f a, f b)
   in (C.formatWith [C.magenta] s) <> (if ra == rb then ra else ((C.formatWith [C.red] ra) <> "  ⇒  " <> C.formatWith [C.green] rb))

desc :: PackageDescription -> PackageDescription -> String
desc = diffTerm "Synopsis: " $ fromShortText . synopsis

ver :: PackageDescription -> PackageDescription -> String
ver = diffTerm "Version: " $ intercalate "." . fmap show . versionNumbers . I.pkgVersion . package

dep :: String -> [String] -> [String] -> String
dep s a b =
  (C.formatWith [C.magenta] s) <> case diffNew of
    [] -> joinToString a
    _ ->
      (C.formatWith [C.indent 4] (joinToString $ fmap (\x -> red (x `elem` diffOld) x) a))
        <> "\n"
        <> replicate 28 '-'
        <> "\n"
        <> (C.formatWith [C.indent 4] (joinToString $ fmap (\x -> green (x `elem` diffNew) x) b))
  where
    diffNew = b \\ a
    diffOld = a \\ b
    joinToString [] = "[]"
    joinToString xs = intercalate "\n    " xs
    red p x = if p then C.formatWith [C.red] x else x
    green p x = if p then C.formatWith [C.green] x else x