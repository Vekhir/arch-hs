{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
--
-- Naming conversion between haskell package in hackage and archlinux community repo.
--
-- To distribute a haskell package to archlinux, the name of package should be changed according to the naming convention:
--
--   (1) for haskell libraries, their names must have @haskell-@ prefix
--
--   (2) for programs, it depends on circumstances
--
--   (3) names should always be in lower case
--
-- However, it's not enough to prefix the string with @haskell-@ and trasform to lower case; in some special situations, the hackage name
-- may have @haskell-@ prefix already, or the case is irregular, thus we have to a name preset, @NAME_PRESET.json@, manually.
-- Once a package distributed to archlinux, whose name conform to above-mentioned situation, the name preset should be upgraded correspondingly.
--
-- @NAME_PRESET.json@ will be loaded during the compilation, generating haskell code to be called in runtime.
--
-- Converting a community name to hackage name following these steps:
--
--   (1) Find if the name preset contains this rule
--   (2) If it contains, then use it; or remove the @haskell-@ prefix
--
-- Converting a hackage name to community name following these steps:
--
--   (1) Find if the name preset contains this rule
--   (2) If it contains, then use it; or add the @haskell-@ prefix
--
-- For details, see the type 'MyName' and type class 'HasMyName' with its instances.
module Distribution.ArchHs.Name
  ( MyName,
    unMyName,
    HasMyName (..),
    NameRep (..),
    mToCommunityName,
    mToHackageName,
    toCommunityName,
    toHackageName,
    isHaskellPackage,
  )
where

import           Data.Char                                     (toLower)
import           Data.String                                   (IsString,
                                                                fromString)
import           Distribution.ArchHs.Internal.NamePresetLoader
import           Distribution.ArchHs.Internal.Prelude
import           Distribution.ArchHs.Types

-- | The representation of a package name.
data NameRep
  = -- |  archlinx community style
    CommunityRep
  | -- | hackage style
    HackageRep

$(loadNamePreset)

-- | Convert a name from community representation to hackage representation, according to the name preset.
-- If the preset doesn't contain this mapping rule, the function will return 'Nothing'.
-- This function is generated from @NAME_PRESET.json@
communityToHackageP :: MyName 'CommunityRep -> Maybe (MyName 'HackageRep)

-- | Convert a name from hackage representation to community representation, according to the name preset.
-- If the preset doesn't contain this mapping rule, the function will return 'Nothing'.
--
-- This function is generated from @NAME_PRESET.json@
hackageToCommunityP :: MyName 'HackageRep -> Maybe (MyName 'CommunityRep)

-- | Special haskell packages in community reop, which should be ignored in the process.
--
-- This function is generated from @NAME_PRESET.json@
falseListP :: [MyName 'CommunityRep]

-- | Community haskell packages of in the name preset.
--
-- This function is generated from @NAME_PRESET.json@
communityListP :: [MyName 'CommunityRep]

-- | A general package name representation.
-- It has a phantom @a@, which indexes this name.
-- Normally, the index should be the data kinds of 'NameRep'.
--
-- In Cabal API, packages' names are represented by the type 'PackageName';
-- in arch-hs, names parsed from @community.db@ are represented by the type 'CommunityName'.
-- It would be tedious to use two converting functions everywhere, so here comes a intermediate data type
-- to unify them, with type level constraints as bonus.
newtype MyName a = MyName
  { -- | Unwrap the value.
    unMyName :: String
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance IsString (MyName a) where
  fromString = MyName

-- | 'HasMyName' indicates that the type @a@ can be converted to 'MyName'.
-- This is where the actually conversion occurs.
class HasMyName a where
  -- | To 'MyName' in hackage style.
  toHackageRep :: a -> MyName 'HackageRep

  -- | To 'MyName' in community style.
  toCommunityRep :: a -> MyName 'CommunityRep

instance HasMyName (MyName 'CommunityRep) where
  toHackageRep = toHackageRep . CommunityName . unMyName
  toCommunityRep = id

instance HasMyName (MyName 'HackageRep) where
  toHackageRep = id
  toCommunityRep = toCommunityRep . mkPackageName . unMyName

instance HasMyName PackageName where
  toHackageRep = MyName . unPackageName
  toCommunityRep = go . unPackageName
    where
      go s = case hackageToCommunityP (MyName s) of
        Just x -> x
        _ ->
          MyName . fmap toLower $
            ( if "haskell-" `isPrefixOf` s
                then s
                else "haskell-" <> s
            )

instance HasMyName CommunityName where
  toHackageRep = go . unCommunityName
    where
      go s = case communityToHackageP (MyName s) of
        Just x -> x
        _      -> MyName $ drop 8 s
  toCommunityRep = MyName . unCommunityName

-- | Back to 'CommunityName'.
mToCommunityName :: MyName 'CommunityRep -> CommunityName
mToCommunityName = CommunityName . unMyName

-- | Back to 'PackageName'.
mToHackageName :: MyName 'HackageRep -> PackageName
mToHackageName = mkPackageName . unMyName

-- | Convert @n@ to 'CommunityName'.
toCommunityName :: HasMyName n => n -> CommunityName
toCommunityName = mToCommunityName . toCommunityRep

-- | Convert @n@ to 'PackageName'.
toHackageName :: HasMyName n => n -> PackageName
toHackageName = mToHackageName . toHackageRep

-- | Judge if a package in archlinux community repo is haskell package.
--
-- i.e. it is in @preset@ or have @haskell-@ prefix, and is not present in @falseList@ of @NAME_PRESET.json@.
isHaskellPackage :: CommunityName -> Bool
isHaskellPackage name =
  let rep = toCommunityRep name
   in (rep `elem` communityListP || "haskell-" `isPrefixOf` (unMyName rep)) && rep `notElem` falseListP
