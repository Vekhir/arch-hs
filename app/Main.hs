{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as G
import qualified Algebra.Graph.Labelled.AdjacencyMap as GL
import Args
import qualified Colourista as C
import Conduit
import Control.Monad (filterM, unless)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, newIORef)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Distribution.ArchHs.Aur (Aur, aurToIO, isInAur)
import Distribution.ArchHs.Community
  ( defaultCommunityPath,
    isInCommunity,
    loadProcessedCommunity,
  )
import Distribution.ArchHs.Core
  ( cabalToPkgBuild,
    getDependencies,
  )
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Hackage
  ( getLatestCabal,
    getPackageFlag,
    insertDB,
    loadHackageDB,
    lookupHackagePath,
    parseCabalFile,
  )
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Local
import Distribution.ArchHs.Name (toCommunityName)
import Distribution.ArchHs.PP
  ( prettyDeps,
    prettyFlagAssignments,
    prettyFlags,
    prettySkip,
    prettySolvedPkgs,
  )
import qualified Distribution.ArchHs.PkgBuild as N
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (depNotInGHCLib, depNotMyself, getTwo, getUrl)
import Distribution.Hackage.DB (HackageDB)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
  )
import System.FilePath (takeFileName)

app ::
  Members '[Embed IO, State (Set.Set PackageName), CommunityEnv, HackageEnv, FlagAssignmentsEnv, DependencyRecord, Trace, Aur, WithMyErr] r =>
  PackageName ->
  FilePath ->
  Bool ->
  [String] ->
  Bool ->
  FilePath ->
  Sem r ()
app target path aurSupport skip uusi metaPath = do
  (deps, ignored) <- getDependencies (fmap mkUnqualComponentName skip) Nothing target
  inCommunity <- isInCommunity target
  when inCommunity $ throw $ TargetExist target ByCommunity

  when aurSupport $ do
    inAur <- isInAur target
    when inAur $ throw $ TargetExist target ByAur

  let grouped = groupDeps deps
      namesFromSolved x = x ^.. each . pkgName <> x ^.. each . pkgDeps . each . depName
      allNames = nubOrd $ namesFromSolved grouped
  communityProvideList <- (<> ghcLibList) <$> filterM isInCommunity allNames
  let fillProvidedPkgs provideList provider = mapC (\x -> if (x ^. pkgName) `elem` provideList then ProvidedPackage (x ^. pkgName) provider else x)
      fillProvidedDeps provideList provider = mapC (pkgDeps %~ each %~ (\y -> if y ^. depName `elem` provideList then y & depProvider ?~ provider else y))
      filledByCommunity =
        runConduitPure $
          yieldMany grouped
            .| fillProvidedPkgs communityProvideList ByCommunity
            .| fillProvidedDeps communityProvideList ByCommunity
            .| sinkList
      toBePacked1 = filledByCommunity ^.. each . filtered (\case ProvidedPackage _ _ -> False; _ -> True)
  (filledByBoth, toBePacked2) <- do
    embed . when aurSupport $ C.infoMessage "Start searching AUR..."
    aurProvideList <- if aurSupport then filterM (\n -> do embed $ C.infoMessage ("Searching " <> T.pack (unPackageName n)); isInAur n) $ toBePacked1 ^.. each . pkgName else return []
    let filledByBoth =
          if aurSupport
            then
              runConduitPure $
                yieldMany filledByCommunity
                  .| fillProvidedPkgs aurProvideList ByAur
                  .| fillProvidedDeps aurProvideList ByAur
                  .| sinkList
            else filledByCommunity
        toBePacked2 =
          if aurSupport
            then filledByBoth ^.. each . filtered (\case ProvidedPackage _ _ -> False; _ -> True)
            else toBePacked1
    return (filledByBoth, toBePacked2)

  embed $ C.infoMessage "Solved target:"
  embed $ putStrLn . prettySolvedPkgs $ filledByBoth

  embed $ C.infoMessage "Recommended package order (from topological sort):"
  let vertexesToBeRemoved = filledByBoth ^.. each . filtered (\case ProvidedPackage _ _ -> True; _ -> False) ^.. each . pkgName
      removeSelfCycle g = foldr (\n acc -> GL.removeEdge n n acc) g $ toBePacked2 ^.. each . pkgName
      newGraph = GL.induce (`notElem` vertexesToBeRemoved) deps
  flattened <- case G.topSort . GL.skeleton $ removeSelfCycle newGraph of
    Left c -> throw . CyclicExist $ toList c
    Right x -> return x
  embed $ putStrLn . prettyDeps . reverse $ flattened
  flags <- filter (\(_, l) -> not $ null l) <$> mapM (\n -> (n,) <$> getPackageFlag n) flattened

  embed $
    unless (null flags) $ do
      C.infoMessage "Detected flags from targets (their values will keep default unless you specify):"
      putStrLn . prettyFlags $ flags

  unless (null path) $
    mapM_
      ( \solved -> do
          pkgBuild <- cabalToPkgBuild solved (Set.toList ignored) uusi
          let pName = "haskell-" <> N._pkgName pkgBuild
              dir = path </> pName
              fileName = dir </> "PKGBUILD"
              txt = N.applyTemplate pkgBuild
          embed $ createDirectoryIfMissing True dir
          embed $ writeFile fileName txt
          embed $ C.infoMessage $ "Write file: " <> T.pack fileName
      )
      toBePacked2

  unless (null metaPath) $ do
    cabal <- getLatestCabal target
    let url = getUrl $ packageDescription cabal
        name = unPackageName target
        template = N.metaTemplate (T.pack url) (T.pack name)
        providedDepends pkg =
          pkg ^. pkgDeps
            ^.. each
              . filtered (\x -> depNotMyself (pkg ^. pkgName) x && depNotInGHCLib x && x ^. depProvider == Just ByCommunity)
        toStr x = "'" <> (unCommunityName . toCommunityName . _depName) x <> "'"
        depends = intercalate " " . nubOrd . fmap toStr . mconcat $ providedDepends <$> toBePacked2
        flattened' = filter (/= target) flattened
        comment =
          if (not $ null flattened')
            then "# Following dependencies are missing in community: " <> (intercalate ", " $ unPackageName <$> flattened')
            else "\n"
        txt = template (T.pack comment) (T.pack depends)
        dir = metaPath </> "haskell-" <> name <> "-meta"
        fileName = dir </> "PKGBUILD"
    embed $ createDirectoryIfMissing True dir
    embed $ writeFile fileName (T.unpack txt)
    embed $ C.infoMessage $ "Write file: " <> T.pack fileName

-----------------------------------------------------------------------------

runApp ::
  HackageDB ->
  CommunityDB ->
  Map.Map PackageName FlagAssignment ->
  Bool ->
  FilePath ->
  IORef (Set.Set PackageName) ->
  Sem '[CommunityEnv, HackageEnv, FlagAssignmentsEnv, DependencyRecord, Trace, State (Set.Set PackageName), Aur, WithMyErr, Embed IO, Final IO] a ->
  IO (Either MyException a)
runApp hackage community flags stdout path ref =
  runFinal
    . embedToFinal
    . errorToIOFinal
    . aurToIO
    . runStateIORef ref
    . runTrace stdout path
    . evalState Map.empty
    . runReader flags
    . runReader hackage
    . runReader community

runTrace :: Member (Embed IO) r => Bool -> FilePath -> Sem (Trace ': r) a -> Sem r a
runTrace stdout path = interpret $ \case
  Trace m -> do
    when stdout (embed $ putStrLn m)
    unless (null path) (embed $ appendFile path (m ++ "\n"))

-----------------------------------------------------------------------------

main :: IO ()
main = printHandledIOException $
  do
    Options {..} <- runArgsParser

    unless (null optFileTrace) $ do
      C.infoMessage $ "Trace will be dumped to " <> T.pack optFileTrace <> "."
      exist <- doesFileExist optFileTrace
      when exist $
        C.warningMessage $ "File " <> T.pack optFileTrace <> " already existed, overwrite it."

    let useDefaultHackage = "YOUR_HACKAGE_MIRROR" `isInfixOf` optHackagePath
        useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath

    when useDefaultHackage $ C.skipMessage "You didn't pass -h, use hackage index file from default path."
    when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default path."

    let isFlagEmpty = Map.null optFlags
        isSkipEmpty = null optSkip

    when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag assignments may make difference in dependency resolving."
    unless isFlagEmpty $ do
      C.infoMessage "You assigned flags:"
      putStrLn . prettyFlagAssignments $ optFlags

    unless isSkipEmpty $ do
      C.infoMessage "You chose to skip:"
      putStrLn $ prettySkip optSkip

    when optAur $ C.infoMessage "You passed -a, searching AUR may takes a long time."

    when optUusi $ C.infoMessage "You passed --uusi, uusi will become makedepends of each package."

    hackage <- loadHackageDB =<< if useDefaultHackage then lookupHackagePath else return optHackagePath
    C.infoMessage "Loading hackage..."

    let isExtraEmpty = null optExtraCabalPath

    unless isExtraEmpty $
      C.infoMessage $ "You added " <> (T.pack . intercalate ", " $ map takeFileName optExtraCabalPath) <> " as extra cabal file(s), starting parsing right now."

    parsedExtra <- mapM parseCabalFile optExtraCabalPath

    let newHackage = foldr insertDB hackage parsedExtra

    community <- loadProcessedCommunity $ if useDefaultCommunity then defaultCommunityPath else optCommunityPath
    C.infoMessage "Loading community.db..."

    C.infoMessage "Start running..."

    empty <- newIORef Set.empty

    runApp newHackage community optFlags optStdoutTrace optFileTrace empty (app optTarget optOutputDir optAur optSkip optUusi optMetaDir) & printAppResult

-----------------------------------------------------------------------------

groupDeps :: GL.AdjacencyMap (Set.Set DependencyType) PackageName -> [SolvedPackage]
groupDeps graph =
  fmap
    ( \(name, deps) ->
        SolvedPackage name $ fmap (uncurry . flip $ SolvedDependency Nothing) deps
    )
    $ result <> aloneChildren
  where
    result =
      fmap ((\(a, b, c) -> (head b, zip a c)) . unzip3)
        . groupBy (\x y -> uncurry (==) (getTwo _2 x y))
        . fmap (_1 %~ Set.toList)
        . GL.edgeList
        $ graph
    parents = fmap fst result
    children = mconcat $ fmap (\(_, ds) -> fmap snd ds) result
    -- Maybe 'G.vertexSet' is a better choice
    aloneChildren = nubOrd $ zip (filter (`notElem` parents) children) (repeat [])
