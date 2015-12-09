{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ACI.Image where

import qualified Codec.Archive.Tar as Tar
import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Text
import Data.Char
import qualified Data.SemVer as SemVer
import qualified Data.Text as T
import GHC.Generics
import Prelude hiding (FilePath, takeWhile)
import System.FilePath

-- | Create an image archive corresponding to the ACI specification.
-- <https://github.com/appc/spec/blob/master/spec/aci.md#app-container-image ACI>
createImage :: FilePath -> FilePath -> [FilePath] -> IO ()
createImage = Tar.create

--------------------------------------------------------------------------------
-- Manifest
--------------------------------------------------------------------------------
-- | Definition for an image manifest.
data ImageManifest = ImageManifest
    { acKind :: ACKind                            -- ^ Must be an ACKind of value __ImageManifest__.
    , acVersion :: SemVer.Version                 -- ^ The version of the schema specification.
    , name :: ACIdentifier                        -- ^ Human readable name for this App Container image.
    } deriving (Eq,Show)

-- JSON representation of an ImageManifest.
instance FromJSON ImageManifest where
    parseJSON (Object x) =
        ImageManifest <$> x .: "acKind" <*> x .: "acVersion" <*> x .: "name"
    parseJSON _ = fail "Expecting an ImageManifest"


instance ToJSON ImageManifest where
    toJSON ImageManifest {..} =
        object
            [ "acKind" .= acKind
            , "acVersion" .= acVersion
            , "name" .= name
            ]


-- Should wrap SemVer.Version in ACVersion newtype.
instance FromJSON SemVer.Version where
    parseJSON = withText "String" $ \txt ->
        let eitherVer = SemVer.fromText txt
        in case eitherVer of
               Right v -> return v
               Left _ -> fail "Expected a version"

instance ToJSON SemVer.Version where
         toJSON v = toJSON $ SemVer.toText v
--------------------------------------------------------------------------------
-- AC Kind
data ACKind = ACKind T.Text
      deriving (Eq,Generic, Show)

instance FromJSON ACKind
instance ToJSON ACKind

-- | TODO : Capture this constraint in the type system.
consAcKind :: T.Text -> Either T.Text ACKind
consAcKind kind =
    case kind of
        "ImageManifest" -> Right $ ACKind kind
        "PodManifest" -> Right $ ACKind kind
        _ -> Left "Unknown kind."

--------------------------------------------------------------------------------
-- | AC identifier type.
newtype ACIdentifier =
    ACIdentifier T.Text
    deriving (Eq, Generic,Show)

instance FromJSON ACIdentifier
instance ToJSON ACIdentifier
--------------------------------------------------------------------------------
-- Labels

{-
--------------------------------------------------------------------------------
-- | Defines the default parameters that can be used to execute this image as an application.
data ACApp = ACApp
    { appExec :: Maybe [T.Text]                     -- ^ Executable with flags to launch.
    , appUser :: T.Text                             -- ^ UID of user the app is to be run as.
    , appGroup :: T.Text                            -- ^ GID of group the app is to be run as.
    , appSupplementaryGIDs :: Maybe [Integer]       -- ^ Additional GID's under whihch the apps processes should run.
    , appEventHandlers :: Maybe [EventHandler]      -- ^ Hooks based on lifecycle events.
    , appWorkDir :: Maybe FilePath                  -- ^ Workig directory of launched application.
    , appEnvironment :: Maybe (M.Map T.Text T.Text) -- ^ The applications environment variables.
    , appIsolators :: Maybe [Isolator]              -- ^ List of isolation steps that SHOULD be applied to the app.
    , appMntPoints :: Maybe [MountPoint]            -- ^ Locations where this app expects external data to be mounted.
    , appPorts :: Maybe [Port]                      -- ^ Ports where this app will be listening on.
    } deriving (Eq,Show)

instance FromJSON ACApp where
    parseJSON (Object x) =
        ACApp <$> x .:? "exec" <*> x .: "user" <*> x .: "group" <*>
        x .:? "supplementaryGids" <*>
        x .:? "eventHandlers" <*>
        x .:? "workingDirectory" <*>
        x .:? "environment" <*>
        x .:? "isolators" <*>
        x .:? "mountPoints" <*>
        x .:? "ports"

data EventHandler = EventHandler
    { ehExec :: [T.Text]         -- ^ Command to run.
    , ehName :: EventHandlerName -- ^ When command is to be executed.
    } deriving (Eq,Show)

data EventHandlerName
    = PreStart
    | PostStop
    | Undefined T.Text
    deriving (Eq,Show)

textToEvhName :: T.Text -> EventHandlerName
textToEvhName name =
    case name of
        "pre-start" -> PreStart
        "post-stop" -> PostStop
        _ -> Undefined name

instance FromJSON EventHandler where
    parseJSON (Object x) =
        EventHandler <$> x .: "exec" <*> (textToEvhName <$> x .: "name")
    parseJSON _ = fail "Expected an EventHandler!"

data Isolator = Isolator
    { isoName :: ACIdentifier -- ^ Name of the isolator to apply.
    , isoValue :: Value       -- ^ Value to apply.
    } deriving (Eq,Show)

instance FromJSON Isolator where
    parseJSON (Object x) = Isolator <$> x .: "name" <*> x .: "value"

data MountPoint = MountPoint
    { mpName :: T.Text   -- ^ Mount point name.
    , mpPath :: FilePath -- ^ Path inside the rootfs.
    , mpReadOnly :: Bool -- ^ Whether th mountpoint is read-only.
    } deriving (Eq,Show)

data ACDependency = ACDependency
    { depImageName :: ACIdentifier                   -- ^ Name of the dependent App Container image.
    , depImageID :: Maybe T.Text                     -- ^ Content hash of the dependency.
    , depLabels :: Maybe (M.Map ACIdentifier T.Text) -- ^ List of labels for the dependency.
    , depSize :: Maybe Integer                       -- ^ The size of the dependency in bytes.
    } deriving (Eq,Show)

data Port = Port
    { portName :: T.Text          -- ^ Descriptive name for this port.
    , portProto :: T.Text         -- ^ Protocol that will be used on this port. t
    , portNum :: T.Text           -- ^ Port number that will be used.
    , portCount :: Maybe Int      -- ^ A range of ports from portNum to portNum + count -1.
    , portSocketActivated :: Bool -- ^ Whether app is socket activated in ports.
    } deriving (Eq,Show)

data ACAnnotation = ACAnnotation
    { annoName :: T.Text -- ^ Name of the annotation.
    , annoValue :: T.Text -- ^ Value of the annotation.
    } deriving (Eq,Show)
-}
--------------------------------------------------------------------------------
-- Parsers
-- | The kinds currently allowed.
acKinds :: [T.Text]
acKinds = ["ImageManifest", "PodManifest"]

-- | Parse a kind.
acKindParser :: Parser ACKind
acKindParser = ACKind <$> choice (fmap string acKinds)

-- | Parse an AC version, must follow the SemVer spec.
acVersionParser :: Parser SemVer.Version
acVersionParser = SemVer.parser

-- Parse an AC Identifier type, see
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
acIdParser :: Parser ACIdentifier
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
