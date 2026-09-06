{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RDepCheck (FailureCounts (..), check, checkReverseDep, printRdepcheckResult) where

import Control.Monad (forM)
import Data.List (partition)
import Distribution.ArchHs.Exception
import Distribution.ArchHs.ExtraDB (versionInExtra)
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP
import Distribution.ArchHs.RDepCheck
import Distribution.ArchHs.Types
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
  Maybe Version ->
  PackageName ->
  Sem r FailureCounts
check mVersion target = do
  reverseDeps <- reverseDependencyRanges target
  versions <- forM mVersion $ \candidate -> do
    rawVersion <- versionInExtra target
    case simpleParsec rawVersion of
      Just current -> pure (current, candidate)
      Nothing -> throw $ VersionNoParse rawVersion
  failures <- forM reverseDeps $ \reverseDep -> do
    let (doc, counts) = checkReverseDep versions reverseDep
    embed $ putDoc $ doc <> line
    pure counts
  pure $ FailureCounts (sum $ newFailures <$> failures) (sum $ oldFailures <$> failures)

checkReverseDep :: Maybe (Version, Version) -> ReverseDep -> (Doc AnsiStyle, FailureCounts)
checkReverseDep versions ReverseDep {..} =
  ( vsep
      ( annMagneta "Reverse dependency" <> colon
          <+> pretty (unArchLinuxName reverseDepName)
          : (rangeDocs reverseDepRanges <> errors)
      ),
    FailureCounts (length newRanges) (length oldRanges)
  )
  where
    (newRanges, oldRanges) =
      case versions of
        Nothing -> ([], [])
        Just (current, candidate) ->
          partition (withinRange current . snd) $ versionFailures (Just candidate) reverseDepRanges
    errors =
      case versions of
        Nothing -> []
        Just (_, candidate) ->
          versionErrors (annRed "rdep:") candidate newRanges
            <> versionErrors (annYellow "rdep-old:") candidate oldRanges

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
  "rdep=" <> pretty newFailures <> comma <+> "rdep-old=" <> pretty oldFailures

rangeDocs :: [(DepSrc, VersionRange)] -> [Doc AnsiStyle]
rangeDocs result =
  [ indent 2 $ pretty s <> colon <+> viaPretty r
    | (s, r) <- result
  ]

versionErrors :: Doc AnsiStyle -> Version -> [(DepSrc, VersionRange)] -> [Doc AnsiStyle]
versionErrors label version result =
  [ indent 2 $
      label
        <+> viaPretty version
        <+> "is outside"
        <+> pretty src
        <+> "range"
        <+> parens (viaPretty range)
    | (src, range) <- result
  ]
