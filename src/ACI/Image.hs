{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ACI.Image where

import qualified Codec.Archive.Tar as Tar
import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import qualified Data.SemVer as SemVer
import qualified Data.Map as M
import qualified Data.Text as T
import Prelude hiding (takeWhile)

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
data ImageManifest = ImageManifest
    { acKind :: ACKind
    , acVersion :: SemVer.Version
    , name :: ACIdentifier
    , labels :: M.Map ACIdentifier T.Text
    , app :: Maybe ACApp
    , dependencies :: ACDependencies
    , annotations :: ACAnnotations
    }

data ACApp = ACApp
    { appExec :: Maybe [T.Text]
      -- ^ Executable with flags to launch.
    , appUser :: T.Text
      -- ^ UID of user the app is to be run as.
    , appGroup :: T.Text
      -- ^ GID of group the app is to be run as.
    , appSupplementaryGIDs :: Maybe [Integer]
      -- ^ Additional GID's under whihch the apps processes should run.
    , appEventHandlers :: Maybe [EventHandler]
      -- ^ Hooks based on lifecycle events.
      
    }

data ACDependencies = ACDependencies

data ACAnnotations = ACAnnotations

data EventHandler = EventHandler
     { ehExec :: [T.Text]
     , ehName :: EventHandlerName
     }

data EventHandlerName = PreStart | PostStop

newtype ACKind = ACKind T.Text deriving (Eq,Show)

-- | AC identifier type.
newtype ACIdentifier = ACIdentifier T.Text deriving (Eq,Show)

-- | The kinds currently allowed.
acKinds
    :: [T.Text]
acKinds = ["ImageManifest", "PodManifest"]

-- | Parse a kind.
acKindParser
    :: Parser ACKind
acKindParser = ACKind <$> choice (fmap string acKinds)

-- | Parse an AC version, must follow the SemVer spec.
acVersionParser
    :: Parser SemVer.Version
acVersionParser = SemVer.parser

-- Parse an AC Identifier type, see
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
acIdParser
    :: Parser ACIdentifier
acIdParser = toAcid <$> initAllowed <*> tailAllowed
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
