{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ACI.Image where

import qualified Codec.Archive.Tar    as Tar
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Proxy
import qualified Data.Text            as T
import           GHC.TypeLits
import           System.IO

import           Prelude              hiding (takeWhile)

--------------------------------------------------------------------------------
-- ACI Image
--------------------------------------------------------------------------------

-- | Create an image archive corresponding to the ACI specification.
-- <https://github.com/appc/spec/blob/master/spec/aci.md#app-container-image ACI>
createImage :: FilePath -> FilePath -> [FilePath] -> IO ()
createImage = Tar.create


--------------------------------------------------------------------------------
-- Manifest
--------------------------------------------------------------------------------

-- | AC Kind Type. Currently there are only two types defined,
-- ImageManifest and PodManifest.
{-
-- Some hacking with type level strings...

newtype ACKindT (s :: Symbol) = ACKindT { acKindTValue :: T.Text } deriving (Eq, Show)

consAcKind :: forall proxy s. KnownSymbol s => proxy s -> ACKindT s
consAcKind _ = ACKindT (T.pack $ symbolVal (Proxy :: Proxy s))

type ImageManifest = ACKindT "ImageManifest"
-}

newtype ACKind = ACKind T.Text deriving (Eq, Show)

acKinds :: [T.Text]
acKinds = ["ImageManifest", "PodManifest"]

acKindParser :: Parser ACKind
acKindParser = ACKind <$> choice (fmap string acKinds)


-- | AC identifier type.
newtype ACIdentifier = ACIdentifier T.Text deriving (Eq, Show)

-- Parse an AC Identifier type, see
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
acidParser :: Parser ACIdentifier
acidParser = ACIdentifier <$> ( initChar >>= parseAndBuild )
  where
    initChar = (T.pack . flip (:) []) <$> choice [letter, digit]
    parseAndBuild c = T.append c <$> takeWhile (inClass "-a-z0-9._/~")

