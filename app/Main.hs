{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as G
import Args
import qualified Colourista as C
import Conduit
import qualified Control.Exception as CE
import Control.Monad (filterM, when)
import Data.List (groupBy, intercalate, isInfixOf, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Distribution.ArchHs.Aur
import Distribution.ArchHs.Community
import Distribution.ArchHs.Core
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Local
import Distribution.ArchHs.PP
import qualified Distribution.ArchHs.PkgBuild as N
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (getTwo)
import Distribution.Hackage.DB (HackageDB)
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Types.PackageName
  ( PackageName,
    unPackageName,
  )
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, (</>))

app ::
  Members '[Embed IO, CommunityEnv, HackageEnv, FlagAssignmentsEnv, DependencyRecord, Aur, WithMyErr] r =>
  PackageName ->
  FilePath ->
  Bool ->
  [String] ->
  Sem r ()
app target path aurSupport skip = do
  deps <- getDependencies Set.empty (fmap mkUnqualComponentName skip) True target
  inCommunity <- isInCommunity target
  when inCommunity $ throw $ TargetExist target ByCommunity

  if aurSupport
    then do
      inAur <- isInAur target
      when inAur $ throw $ TargetExist target ByAur
    else return ()

  let grouped = groupDeps deps
      namesFromSolved x = x ^.. each . pkgName <> x ^.. each . pkgDeps . each . depName
      allNames = nub $ namesFromSolved grouped
  communityProvideList <- (<> ghcLibList) <$> filterM isInCommunity allNames
  let fillProvidedPkgs provideList provider = mapC (\x -> if (x ^. pkgName) `elem` provideList then ProvidedPackage (x ^. pkgName) provider else x)
      fillProvidedDeps provideList provider = mapC (pkgDeps %~ each %~ (\y -> if y ^. depName `elem` provideList then y & depProvider .~ (Just provider) else y))
      cooked =
        runConduitPure $
          yieldMany grouped
            .| fillProvidedPkgs communityProvideList ByCommunity
            .| fillProvidedDeps communityProvideList ByCommunity
            .| sinkList
      toBePacked = cooked ^.. each . filtered (\case ProvidedPackage _ _ -> False; _ -> True)
  (cookedAgain, toBePackedAgain) <- do
    embed . when aurSupport $ C.infoMessage "Start searching AUR..."
    aurProvideList <- if aurSupport then filterM (\n -> do embed $ C.infoMessage ("Searching " <> (T.pack $ unPackageName n)); isInAur n) $ toBePacked ^.. each . pkgName else return []
    let cookedAgain =
          if aurSupport
            then
              runConduitPure $
                yieldMany cooked
                  .| fillProvidedPkgs aurProvideList ByAur
                  .| fillProvidedDeps aurProvideList ByAur
                  .| sinkList
            else cooked
        toBePackedAgain =
          if aurSupport
            then cookedAgain ^.. each . filtered (\case ProvidedPackage _ _ -> False; _ -> True)
            else toBePacked
    return (cookedAgain, toBePackedAgain)

  embed $ C.infoMessage "Solved target:"
  embed $ putStrLn . prettySolvedPkgs $ cookedAgain

  let flattened = filter (`elem` (toBePackedAgain ^.. each . pkgName)) $ G.reachable target $ G.skeleton deps
  embed $ C.infoMessage "Recommended package order (from topological sort):"
  embed $ putStrLn . prettyDeps $ flattened
  flags <- filter (\(_, l) -> length l /= 0) <$> mapM (\n -> (n,) <$> getPackageFlag n) flattened

  embed $
    when (length flags /= 0) $ do
      C.infoMessage "Detected flags from targets (their values will keep default unless you specify):"
      putStrLn . prettyFlags $ flags

  let dry = path == ""
  embed $ when dry $ C.warningMessage "You didn't pass -o, PKGBUILD files will not be generated."
  when (not dry) $
    mapM_
      ( \solved -> do
          pkgBuild <- cabalToPkgBuild solved
          let pName = "haskell-" <> N._pkgName pkgBuild
              dir = path </> pName
              fileName = dir </> "PKGBUILD"
              txt = N.applyTemplate pkgBuild
          embed $ createDirectoryIfMissing True dir
          embed $ writeFile fileName txt
          embed $ C.infoMessage $ "Write file: " <> T.pack fileName
      )
      toBePackedAgain

runApp ::
  HackageDB ->
  CommunityDB ->
  Map.Map PackageName FlagAssignment ->
  Sem '[CommunityEnv, HackageEnv, FlagAssignmentsEnv, DependencyRecord, Aur, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runApp hackage community flags =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . aurToIO
    . evalState Map.empty
    . runReader flags
    . runReader hackage
    . runReader community

main :: IO ()
main = CE.catch @CE.IOException
  ( do
      Options {..} <- runArgsParser

      let useDefaultHackage = isInfixOf "YOUR_HACKAGE_MIRROR" $ optHackagePath
          useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath

      when useDefaultHackage $ C.skipMessage "You didn't pass -h, use hackage index file from default places."
      when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default places."

      let isFlagEmpty = optFlags == Map.empty
          isSkipEmpty = optSkip == []

      when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag values may make difference in dependency resolving."
      when (not isFlagEmpty) $ do
        C.infoMessage "You assigned flags:"
        putStrLn . prettyFlagAssignments $ optFlags

      when (not isSkipEmpty) $ do
        C.infoMessage "You chose to skip:"
        putStrLn $ prettySkip optSkip

      when optAur $ C.infoMessage "You passed -a, searching AUR may takes a long time."

      hackage <- loadHackageDB =<< if useDefaultHackage then lookupHackagePath else return optHackagePath
      C.infoMessage "Loading hackage..."

      let isExtraEmpty = optExtraCabalPath == []

      when (not isExtraEmpty) $
        C.infoMessage $ "You added " <> (T.pack . intercalate ", " $ map takeFileName optExtraCabalPath) <> " as extra cabal file(s), starting parsing right now."

      parsedExtra <- mapM parseCabalFile optExtraCabalPath

      let newHackage = foldr (\x acc -> x `insertDB` acc) hackage parsedExtra

      community <- loadProcessedCommunity $ if useDefaultCommunity then defaultCommunityPath else optCommunityPath
      C.infoMessage "Loading community.db..."

      C.infoMessage "Start running..."

      runApp newHackage community optFlags (app optTarget optOutputDir optAur optSkip) >>= \case
        Left x -> C.errorMessage $ "Runtime Error: " <> (T.pack . show $ x)
        _ -> C.successMessage "Success!"
  )
  $ \e -> C.errorMessage $ "IOException: " <> (T.pack . show $ e)

-----------------------------------------------------------------------------

groupDeps :: G.AdjacencyMap (Set.Set DependencyType) PackageName -> [SolvedPackage]
groupDeps =
  fmap (\(name, deps) -> SolvedPackage name $ fmap (uncurry . flip $ SolvedDependency Nothing) deps)
    . fmap ((\(a, b, c) -> (head b, zip a c)) . unzip3)
    . groupBy (\x y -> uncurry (==) (getTwo _2 x y))
    . fmap (_1 %~ Set.toList)
    . Set.toList
    . G.edgeSet
