module Args
  ( Options (..),
    cmdOptions,
    runArgsParser,
  )
where

import Distribution.ArchHs.Internal.Prelude
import Distribution.ArchHs.Options
import Distribution.ArchHs.Types
import Distribution.ArchHs.Utils (archHsVersion)

data Options = Options
  { optFlags :: FlagAssignments,
    optExtraDB :: ExtraDBOptions,
    optHackage :: HackageDBOptions,
    optPackageName :: PackageName,
    optCheckVersion :: Maybe Version
  }

cmdOptions :: Parser Options
cmdOptions =
  Options
    <$> optFlagAssignmentParser
    <*> extraDBOptionsParser
    <*> hackageDBOptionsParser
    <*> argument optPackageNameReader (metavar "TARGET")
    <*> optional (argument optVersionReader (metavar "VERSION"))

runArgsParser :: IO Options
runArgsParser = do
  (x, ()) <-
    simpleOptions
      archHsVersion
      "arch-hs-rdepcheck - inspect reverse dependency version ranges"
      "arch-hs-rdepcheck shows all reverse dependencies of a Haskell package in [extra] and the version ranges they require. If VERSION is provided, it counts newly unmet ranges as rdep and already unmet ranges as rdep-old, and exits with failure only for newly unmet ranges."
      cmdOptions
      empty
  pure x
