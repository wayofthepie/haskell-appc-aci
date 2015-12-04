{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module ACI.Image where

import qualified Codec.Archive.Tar    as Tar
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text            as T
import           Debug.Trace
import           GHC.TypeLits
import           Prelude              hiding (takeWhile)
import           System.IO

--------------------------------------------------------------------------------
-- ACI Image
-- -----------------------------------------------------------------------------
-- | Create an image archive corresponding to the ACI specification.
-- <https://github.com/appc/spec/blob/master/spec/aci.md#app-container-image ACI>
createImage
    :: FilePath -> FilePath -> [FilePath] -> IO ()
createImage = Tar.create

--------------------------------------------------------------------------------
-- Manifest
--------------------------------------------------------------------------------
newtype ACKind =
    ACKind T.Text
    deriving (Eq,Show)

-- | The kinds currently allowed.
acKinds
    :: [T.Text]
acKinds = ["ImageManifest", "PodManifest"]

-- | Parse a kind.
acKindParser
    :: Parser ACKind
acKindParser = ACKind <$> choice (fmap string acKinds)

-- | AC identifier type.
newtype ACIdentifier =
    ACIdentifier T.Text
    deriving (Eq,Show)

-- Parse an AC Identifier type, see
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
acidParser
    :: Parser ACIdentifier
acidParser = toAcid <$> initAllowed <*> tailAllowed
  where
    initAllowed = choice [letter, digit]
    tailAllowed = do
        t <- takeWhile (inClass "-a-z0-9._/~")
        let endc = T.last t
        if not (isLetter endc || isNumber endc)
            then fail
                     ("Expected to end with a character or a digit, found " ++
                      [endc])
            else return t
    toAcid ic s = ACIdentifier $ T.cons ic s
