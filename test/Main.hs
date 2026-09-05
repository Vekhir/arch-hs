{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Check as Sync
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Diff (inRange)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (defaultExtraDBPath, loadExtraDB)
import Distribution.ArchHs.Hackage (getCabalIncludingDeprecated, getNewerVersions)
import Distribution.ArchHs.Name (isGHCLibs, isHaskellPackage, toArchLinuxName, toHackageName)
import Distribution.ArchHs.Types
import qualified Distribution.Hackage.DB.Parsed as Hackage
import qualified Distribution.Hackage.DB.Unparsed as RawHackage
import Distribution.Package (packageName, packageVersion)
import Distribution.PackageDescription (GenericPackageDescription, packageDescription)
import Distribution.Parsec (simpleParsec)
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange, anyVersion)
import Polysemy (run, runM)
import Polysemy.Error (runError)
import Polysemy.Reader (runReader)
import Polysemy.State (evalState)
import Polysemy.Trace (ignoreTrace)
import Submit.CSV
import System.Directory (doesFileExist)
import Test.Hspec
import Utils (linkedHaskellPackageDescs)

main :: IO ()
main = hspec $ do
  describe "live pacman extra database" $ do
    it "loads current extra.db and finds parseable Haskell package metadata" $ do
      extra <- loadLiveExtraDB
      samples <- requireHaskellSamples extra

      forM_ samples $ \(archName, desc@PkgDesc {_version}) -> do
        _name desc `shouldBe` archName
        _version `shouldSatisfy` not . null
        unPackageName (toHackageName archName) `shouldSatisfy` not . null
        archName `shouldSatisfy` isHaskellPackage

    it "keeps raw Arch EVR for a real Haskell package with pkgrel" $ do
      extra <- loadLiveExtraDB
      (_, PkgDesc {_version, _rawVersion}) <- requireHaskellPackage extra
      _rawVersion `shouldSatisfy` (_version `isPrefixOf`)
      drop (length _version) _rawVersion `shouldSatisfy` isPkgrelSuffix

    it "renders Hackage distro CSV rows from current extra.db and parses them back" $ do
      extra <- loadLiveExtraDB
      let rows = take 20 $ liveDistroRows extra
      rows `shouldSatisfy` not . null
      parseDistroCSV (renderDistroCSV rows) `shouldBe` rows

  describe "diff dependency range checks" $ do
    it "uses a live extra.db version when checking a Hackage dependency range" $ do
      extra <- loadLiveExtraDB
      (hackageName, expectedVersion) <- requireParseableHackagePackage extra

      case runInRange extra hackageName anyVersion of
        Right (Right (actualName, _, actualVersion, inBounds)) -> do
          actualName `shouldBe` hackageName
          actualVersion `shouldBe` expectedVersion
          inBounds `shouldBe` True
        result ->
          expectationFailure $ "expected successful range check, got: " <> show result

    it "reports a malformed live package version as VersionNoParse instead of crashing" $ do
      extra <- loadLiveExtraDB
      (archName, desc) <- requireHaskellPackage extra
      let hackageName = toHackageName archName
          badVersion = _version desc <> "_not_cabal"
          poisonedExtra = Map.insert archName (desc {_version = badVersion}) extra

      case runInRange poisonedExtra hackageName anyVersion of
        Left (VersionNoParse rawVersion) ->
          rawVersion `shouldBe` badVersion
        result ->
          expectationFailure $ "expected VersionNoParse, got: " <> show result

  describe "Hackage preferred-version handling" $ do
    it "does not report masked versions as newer candidates" $ do
      let (preferred, _, name, version100, version101) = maskedHackageDBs
      assertGetNewerVersions preferred name version100 []
      assertGetNewerVersions preferred name version101 []

    it "can still load an exact masked cabal from the raw Hackage DB" $ do
      let (_, raw, name, _, version101) = maskedHackageDBs
      case runGetCabalIncludingDeprecated raw name version101 of
        Right cabal -> do
          let desc = packageDescription cabal
          packageName desc `shouldBe` name
          packageVersion desc `shouldBe` version101
        Left err ->
          expectationFailure $ "expected masked cabal lookup to succeed, got: " <> show err

  describe "sync with unsupported cabal formats" $ do
    it "lists newer versions without parsing their cabal files" $ do
      let (preferred, _, _, name) = unsupportedHackageDBs
      assertGetNewerVersions preferred name (parseVersion "1.0") [parseVersion "1.1", parseVersion "2.0.0"]

    it "links packages by index name without parsing the latest cabal file" $ do
      let (preferred, _, extra, name) = unsupportedHackageDBs
      linked <- runM . runReader preferred . runReader extra $ linkedHaskellPackageDescs
      [(archName, _version desc, hackageName) | (archName, desc, hackageName) <- linked]
        `shouldBe` [(toArchLinuxName name, "1.0", name)]

    it "finishes a version check with an unsupported latest cabal file" $ do
      let (preferred, _, extra, _) = unsupportedHackageDBs
      result <- runSyncCheck extra preferred Map.empty False
      show result `shouldBe` "Right ()"

    it "continues dependency checking past an unsupported cabal file" $ do
      let (preferred, raw, extra, _) = unsupportedHackageDBs
      result <- runSyncCheck extra preferred raw True
      show result `shouldBe` "Right ()"

loadLiveExtraDB :: IO ExtraDB
loadLiveExtraDB = do
  exists <- doesFileExist defaultExtraDBPath
  if exists
    then loadExtraDB defaultExtraDBPath
    else skip $ "pacman database not found: " <> defaultExtraDBPath

requireHaskellSamples :: ExtraDB -> IO [(ArchLinuxName, PkgDesc)]
requireHaskellSamples extra = do
  let samples = take 20 . sortOn (unArchLinuxName . fst) $ haskellPackages extra
  if null samples
    then skip "current extra.db has no Haskell packages"
    else pure samples

requireHaskellPackage :: ExtraDB -> IO (ArchLinuxName, PkgDesc)
requireHaskellPackage extra =
  case listToMaybe $ haskellPackages extra of
    Just sample -> pure sample
    Nothing -> skip "current extra.db has no Haskell packages"

requireParseableHackagePackage :: ExtraDB -> IO (PackageName, Version)
requireParseableHackagePackage extra =
  case listToMaybe $ parseableHaskellPackages extra of
    Just sample -> pure sample
    Nothing -> skip "current extra.db has no Haskell packages with Cabal-style versions"

isPkgrelSuffix :: String -> Bool
isPkgrelSuffix ('-' : rest) = not (null rest)
isPkgrelSuffix _ = False

haskellPackages :: ExtraDB -> [(ArchLinuxName, PkgDesc)]
haskellPackages =
  filter (isHaskellPackage . fst) . Map.toList

parseableHaskellPackages :: ExtraDB -> [(PackageName, Version)]
parseableHaskellPackages =
  mapMaybe
    ( \(archName, PkgDesc {_version}) ->
        case simpleParsec _version of
          Just version -> Just (toHackageName archName, version)
          Nothing -> Nothing
    )
    . haskellPackages

liveDistroRows :: ExtraDB -> DistroCSV
liveDistroRows =
  sortOn
    (\(name, _, _) -> name)
    . fmap toRow
    . haskellPackages
  where
    toRow (archName, PkgDesc {_version}) =
      ( unPackageName hackageName,
        _version,
        "https://archlinux.org/packages/extra/x86_64/" <> packagePath hackageName archName
      )
      where
        hackageName = toHackageName archName

    packagePath hackageName archName
      | isGHCLibs hackageName = "ghc"
      | otherwise = unArchLinuxName archName

runInRange :: ExtraDB -> PackageName -> VersionRange -> Either MyException (Either (PackageName, VersionRange) (PackageName, VersionRange, Version, Bool))
runInRange extra name range =
  run
    . runError @MyException
    . runReader extra
    $ inRange (name, range)

maskedHackageDBs :: (Hackage.HackageDB, RawHackage.HackageDB, PackageName, Version, Version)
maskedHackageDBs =
  (Hackage.parseDB raw, raw, name, version100, version101)
  where
    name = mkPackageName "masked"
    version100 = parseVersion "1.0.0"
    version101 = parseVersion "1.0.1"
    raw =
      Map.singleton
        name
        ( RawHackage.PackageData
            (B8.pack "masked <1.0.1 || >1.0.1")
            ( Map.fromList
                [ (version100, RawHackage.VersionData (cabalFile "1.0.0") (B8.pack "{}")),
                  (version101, RawHackage.VersionData (cabalFile "1.0.1") (B8.pack "{}"))
                ]
            )
        )

    cabalFile version =
      B8.pack $
        unlines
          [ "cabal-version: 1.12",
            "name: masked",
            "version: " <> version,
            "build-type: Simple"
          ]

parseVersion :: String -> Version
parseVersion raw =
  case simpleParsec raw of
    Just version -> version
    Nothing -> error $ "test fixture version does not parse: " <> raw

runGetNewerVersions :: Hackage.HackageDB -> PackageName -> Version -> Either MyException [Version]
runGetNewerVersions hackage name version =
  run
    . runError @MyException
    . runReader hackage
    $ getNewerVersions name version

assertGetNewerVersions :: Hackage.HackageDB -> PackageName -> Version -> [Version] -> Expectation
assertGetNewerVersions hackage name version expected =
  case runGetNewerVersions hackage name version of
    Right actual -> actual `shouldBe` expected
    Left err -> expectationFailure $ "expected newer versions, got: " <> show err

runGetCabalIncludingDeprecated :: RawHackage.HackageDB -> PackageName -> Version -> Either MyException GenericPackageDescription
runGetCabalIncludingDeprecated hackage name version =
  run
    . runError @MyException
    . runReader hackage
    $ getCabalIncludingDeprecated name version

unsupportedHackageDBs :: (Hackage.HackageDB, RawHackage.HackageDB, ExtraDB, PackageName)
unsupportedHackageDBs =
  (Hackage.parseDB raw, raw, Map.singleton archName desc, name)
  where
    name = mkPackageName "Diff"
    archName = toArchLinuxName name
    raw =
      Map.singleton name $
        RawHackage.PackageData
          (B8.pack "Diff <3")
          ( Map.fromList
              [ (parseVersion "1.1", versionData "1.12" "1.1"),
                (parseVersion "2.0.0", versionData "999.0" "2.0.0"),
                (parseVersion "3.0", versionData "999.0" "3.0")
              ]
          )

    -- Use a future format so the regression survives upgrades of Cabal itself.
    versionData format version =
      RawHackage.VersionData
        ( B8.pack $
            unlines
              [ "cabal-version: " <> format,
                "name: Diff",
                "version: " <> version,
                "build-type: Simple"
              ]
        )
        B8.empty

    desc =
      PkgDesc
        { _name = archName,
          _version = "1.0",
          _rawVersion = "1.0-1",
          _desc = "Diff algorithm in pure Haskell",
          _url = Nothing,
          _provides = [],
          _optDepends = [],
          _replaces = [],
          _conflicts = [],
          _depends = [],
          _makeDepends = [],
          _checkDepends = []
        }

runSyncCheck :: ExtraDB -> Hackage.HackageDB -> RawHackage.HackageDB -> Bool -> IO (Either MyException ())
runSyncCheck extra hackage raw depCheck =
  runM
    . runError @MyException
    . evalState (Map.empty :: Map.Map PackageName [VersionRange])
    . ignoreTrace
    . runReader (Map.empty :: FlagAssignments)
    . runReader (parseVersion "9.6.6")
    . runReader raw
    . runReader hackage
    . runReader extra
    $ Sync.check False depCheck True

skip :: String -> IO a
skip reason = pendingWith reason >> error "unreachable"
