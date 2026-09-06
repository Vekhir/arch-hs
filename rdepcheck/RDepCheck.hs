{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module RDepCheck (FailureCounts (..), check, checkReverseDep, checkReverseDepRevisions, printRdepcheckResult) where

import Control.Monad (forM)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (versionInExtra)
import Distribution.ArchHs.Hackage (RawHackageDB)
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.RDepCheck
import Distribution.ArchHs.Types
import Distribution.Version (asVersionIntervals)
import System.Exit (exitFailure)

data FailureCounts = FailureCounts
  { newFailures :: Int,
    oldFailures :: Int
  }
  deriving stock (Eq, Show)

check ::
  Members
    [ ExtraEnv,
      RawHackageEnv,
      KnownGHCVersion,
      FlagAssignmentsEnv,
      Trace,
      DependencyRecord,
      WithMyErr,
      Embed IO
    ]
    r =>
  RawHackageDB ->
  Maybe Version ->
  PackageName ->
  Sem r FailureCounts
check revision0 mVersion target = do
  latest <- indexResults <$> reverseDependencyRangesWithSkips target
  original <- indexResults <$> local @RawHackageDB (const revision0) (reverseDependencyRangesWithSkips target)
  versions <- forM mVersion $ \candidate -> do
    rawVersion <- versionInExtra target
    case simpleParsec rawVersion of
      Just current -> pure (current, candidate)
      Nothing -> throw $ VersionNoParse rawVersion
  failures <- forM (Map.toList latest) $ \(name, latestResult) -> do
    let originalResult = Map.findWithDefault (Left $ PkgNotFound name) name original
        (doc, counts) = checkReverseDepRevisions versions name latestResult originalResult
    embed $ putDoc $ doc <> line
    pure counts
  pure $ FailureCounts (sum $ newFailures <$> failures) (sum $ oldFailures <$> failures)

indexResults :: ([ReverseDep], [SkippedReverseDep]) -> Map.Map ArchLinuxName (Either MyException ReverseDep)
indexResults (checked, skipped) =
  Map.fromList $
    [(reverseDepName dep, Right dep) | dep <- checked]
      <> [(skippedReverseDepName dep, Left $ skippedReverseDepError dep) | dep <- skipped]

checkReverseDep :: Maybe (Version, Version) -> ReverseDep -> (Doc AnsiStyle, FailureCounts)
checkReverseDep versions ReverseDep {..} =
  let (docs, counts) = checkRanges versions reverseDepRanges
   in (vsep $ reverseDepHeader reverseDepName : docs, counts)

checkReverseDepRevisions ::
  Maybe (Version, Version) ->
  ArchLinuxName ->
  Either MyException ReverseDep ->
  Either MyException ReverseDep ->
  (Doc AnsiStyle, FailureCounts)
checkReverseDepRevisions versions name latest original
  | sameResult latest original =
      case latest of
        Right dep -> checkReverseDep versions dep
        Left err -> (annYellow $ "Skip" <+> pretty (unArchLinuxName name) <> colon <+> viaShow err, FailureCounts 0 0)
  | otherwise =
      ( vsep $
          reverseDepHeader name
            : revisionDocs annCyan "latest revision" latest
              <> revisionDocs annBlue "revision 0" original,
        snd $ resultDetails latest
      )
  where
    sameResult (Right a) (Right b) =
      [(src, asVersionIntervals range) | (src, range) <- reverseDepRanges a]
        == [(src, asVersionIntervals range) | (src, range) <- reverseDepRanges b]
    sameResult (Left a) (Left b) = show a == show b
    sameResult _ _ = False

    resultDetails (Right dep) = checkRanges versions $ reverseDepRanges dep
    resultDetails (Left err) = ([indent 2 $ annYellow $ "unchecked:" <+> viaShow err], FailureCounts 0 0)

    revisionDocs style label result =
      let (docs, counts) = resultDetails result
          status = case (versions, result) of
            (Just _, Right _) -> space <> parens (prettyFailureCounts counts)
            _ -> mempty
       in indent 2 (style $ annBold label <> status <> colon) : fmap (indent 2) docs

reverseDepHeader :: ArchLinuxName -> Doc AnsiStyle
reverseDepHeader name = annMagneta $ "Reverse dependency" <> colon <+> annBold (pretty $ unArchLinuxName name)

checkRanges :: Maybe (Version, Version) -> [(DepSrc, VersionRange)] -> ([Doc AnsiStyle], FailureCounts)
checkRanges versions ranges =
  ( rangeDocs versions ranges <> errors,
    FailureCounts (length newRanges) (length oldRanges)
  )
  where
    (newRanges, oldRanges) =
      case versions of
        Nothing -> ([], [])
        Just (current, candidate) ->
          partition (withinRange current . snd) $ versionFailures (Just candidate) ranges
    errors =
      case versions of
        Nothing -> []
        Just (_, candidate) ->
          versionErrors annRed "rdep:" candidate newRanges
            <> versionErrors annYellow "rdep-old:" candidate oldRanges

printRdepcheckResult :: IO (Either MyException FailureCounts) -> IO ()
printRdepcheckResult io = do
  result <- io
  case result of
    Left err -> do
      printError $ "Runtime Exception" <> colon <+> viaShow err
      exitFailure
    Right counts@FailureCounts {..}
      | newFailures > 0 -> do
          printError $ "Reverse dependency range check(s) failed:" <+> prettyFailureCounts counts
          exitFailure
      | oldFailures > 0 ->
          printWarn $ "Existing reverse dependency range failure(s):" <+> prettyFailureCounts counts
      | otherwise -> printSuccess "Success!"

prettyFailureCounts :: FailureCounts -> Doc AnsiStyle
prettyFailureCounts FailureCounts {..} =
  (if newFailures == 0 then annGreen else annRed) ("rdep=" <> pretty newFailures)
    <> comma
      <+> (if oldFailures == 0 then annGreen else annYellow) ("rdep-old=" <> pretty oldFailures)

rangeDocs :: Maybe (Version, Version) -> [(DepSrc, VersionRange)] -> [Doc AnsiStyle]
rangeDocs versions result =
  [ indent 2 $ pretty s <> colon <+> rangeColor r (viaPretty r)
    | (s, r) <- result
  ]
  where
    rangeColor range =
      case versions of
        Nothing -> annBlue
        Just (current, candidate)
          | withinRange candidate range -> annGreen
          | withinRange current range -> annRed
          | otherwise -> annYellow

versionErrors :: (Doc AnsiStyle -> Doc AnsiStyle) -> Doc AnsiStyle -> Version -> [(DepSrc, VersionRange)] -> [Doc AnsiStyle]
versionErrors style label version result =
  [ indent 2 $ style $
      label
        <+> annBold (viaPretty version)
        <+> "is outside"
        <+> pretty src
        <+> "range"
        <+> parens (viaPretty range)
    | (src, range) <- result
  ]
