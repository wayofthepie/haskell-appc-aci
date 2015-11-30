{-# LANGUAGE
     DataKinds
    , GADTs
    , OverloadedStrings
    , ScopedTypeVariables
    , TypeFamilies
    #-}
module ACI.Image where

import qualified Codec.Archive.Tar as Tar
import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Text
import qualified Data.Text as T
import System.IO

import Prelude hiding (takeWhile)

-- * ACI Image

-- | AC identifier type.
newtype ACId = ACId T.Text

-- Parse an AC Identifier type, see
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
acidParser :: Parser ACId
acidParser = ACId <$> ( initChar >>= parseAndBuild )
  where
    initChar = (T.pack . flip (:) []) <$> choice [letter, digit]
    parseAndBuild c = T.append c <$>  (takeWhile $ inClass "a-z0-9./~")



-- | Create an image archive corresponding to the ACI specification.
-- <https://github.com/appc/spec/blob/master/spec/aci.md#app-container-image ACI>
createImage :: FilePath -> FilePath -> [FilePath] -> IO ()
createImage = Tar.create


