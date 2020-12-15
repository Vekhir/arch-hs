{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Colourista as C
import Control.Monad (unless)
import qualified Data.Map as Map
import qualified Data.Text as T
import Diff
import Distribution.ArchHs.CommunityDB
import Distribution.ArchHs.Exception
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.PP (prettyFlagAssignments)
import Distribution.ArchHs.Types

main :: IO ()
main = printHandledIOException $
  do
    Options {..} <- runArgsParser
    let isFlagEmpty = Map.null optFlags

#ifndef ALPM
    let useDefaultCommunity = "/var/lib/pacman/sync/community.db" == optCommunityPath
    when useDefaultCommunity $ C.skipMessage "You didn't pass -c, use community db file from default path."
#endif

    when isFlagEmpty $ C.skipMessage "You didn't pass -f, different flag values may make difference in dependency resolving."
    unless isFlagEmpty $ do
      C.infoMessage "You assigned flags:"
      putStrLn . prettyFlagAssignments $ optFlags

#ifdef ALPM
    when optAlpm $ C.infoMessage "Using alpm."
    community <- if optAlpm then loadCommunityDBFFI else loadProcessedCommunityDB defaultCommunityDBPath
#else
    community <- loadProcessedCommunityDB $ if useDefaultCommunity then defaultCommunityDBPath else optCommunityPath
#endif
    
    C.infoMessage "Loading community.db..."

    C.infoMessage "Start running..."
    runDiff community optFlags (diffCabal optPackageName optVersionA optVersionB) >>= \case
      Left x -> C.errorMessage $ "Runtime Error: " <> (T.pack . show $ x)
      Right r -> putStrLn r >> C.successMessage "Success!"

runDiff :: CommunityDB -> FlagAssignments -> Sem '[CommunityEnv, FlagAssignmentsEnv, Trace, DependencyRecord, WithMyErr, Embed IO, Final IO] a -> IO (Either MyException a)
runDiff community flags = runFinal . embedToFinal . errorToIOFinal . evalState Map.empty . ignoreTrace . runReader flags . runReader community
