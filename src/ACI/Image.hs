{-# LANGUAGE
     DataKinds
    , GADTs
    , OverloadedStrings
    , ScopedTypeVariables
    , TypeFamilies
    #-}
module ACI.Image (
    -- * ACI Image
    createImage

    -- ** Manifest
    , ACIdentifier(..)
    , acidParser
    )where

import qualified Codec.Archive.Tar as Tar
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T
import System.IO

import Prelude hiding (takeWhile)

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

-- | AC identifier type.
newtype ACIdentifier = ACIdentifier T.Text deriving (Eq, Show)

-- Parse an AC Identifier type, see
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
acidParser :: Parser ACIdentifier
acidParser = ACIdentifier <$> ( initChar >>= parseAndBuild )
  where
    initChar = (T.pack . flip (:) []) <$> choice [letter, digit]
    parseAndBuild c = T.append c <$> (takeWhile $ inClass "-a-z0-9._/~")



