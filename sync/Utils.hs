{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils
  ( linkedHaskellPackages,
    linkedHaskellPackageDescs,
  )
where

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Distribution.ArchHs.Hackage
import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Name
import Distribution.ArchHs.PP
import Distribution.ArchHs.Types

linkedHaskellPackages ::
  Members [ExtraEnv, HackageEnv, Embed IO] r =>
  Sem r [(ArchLinuxName, ArchLinuxVersion, PackageName)]
linkedHaskellPackages =
  fmap (\(name, desc, hName) -> (name, _version desc, hName)) <$> linkedHaskellPackageDescs

linkedHaskellPackageDescs ::
  Members [ExtraEnv, HackageEnv, Embed IO] r =>
  Sem r [(ArchLinuxName, PkgDesc, PackageName)]
linkedHaskellPackageDescs = do
  extraHaskellPackages <- filter (isHaskellPackage . fst) . Map.toList <$> ask @ExtraDB
  hackage <- ask @HackageDB
  -- Linking needs only index keys; newer cabal formats may not be parseable.
  let go xs ys ((name, desc) : pkgs) =
        let hName = toHackageName name
         in if Map.member hName hackage
              then go ((name, desc, hName) : xs) ys pkgs
              else go xs (name : ys) pkgs
      go xs ys [] = pure (xs, ys)
  (linked, unlinked) <- go [] [] extraHaskellPackages
  embed $
    unless (null unlinked) $ do
      printWarn $ "Following packages in" <+> ppExtra <+> "are not linked to hackage:"
      putStrLn . unlines $ unArchLinuxName <$> unlinked
  pure linked
