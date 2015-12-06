{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ACI.Image where

import qualified Codec.Archive.Tar as Tar
import           Control.Applicative
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Map as M
import qualified Data.SemVer as SemVer
import qualified Data.Text as T
import           Prelude hiding (FilePath, takeWhile)
import           System.FilePath

-- | Create an image archive corresponding to the ACI specification.
-- <https://github.com/appc/spec/blob/master/spec/aci.md#app-container-image ACI>
createImage :: FilePath -> FilePath -> [FilePath] -> IO ()
createImage = Tar.create

-- | Definition for an image manifest.
data ImageManifest =
       ImageManifest
         {
         -- | Must be an ACKind of value __ImageManifest__.
         acKind :: ACKind
         -- | The version of the schema specification.
         , acVersion :: SemVer.Version
         -- | Human readable name for this App Container image.
         , name :: ACIdentifier
         -- | Labels used during lookup in image discovery.
         , labels :: M.Map ACIdentifier T.Text
         -- | Default parameters that can be used to execute this image as an application.
         , app :: Maybe ACApp
         -- | Dependent application images that need to be placed down into the rootfs before the files from
         -- this image
         , dependencies :: Maybe [ACDependency]
         -- | Whitelist of absolute paths that will exist in the app's rootfs after rendering.
         , pathWhiteList :: Maybe [FilePath]
         -- | Any extra metadata you wish to add to the image.
         , annotations :: Maybe [ACAnnotation]
         }
  deriving (Eq, Show)

-- | Defines the default parameters that can be used to execute this image as an application.
data ACApp =
       ACApp
         {
         -- | Executable with flags to launch.
         appExec :: Maybe [T.Text]
         -- | UID of user the app is to be run as.
         , appUser :: T.Text
         -- | GID of group the app is to be run as.
         , appGroup :: T.Text
         -- | Additional GID's under whihch the apps processes should run.
         , appSupplementaryGIDs :: Maybe [Integer]
         -- | Hooks based on lifecycle events.
         , appEventHandlers :: Maybe [EventHandler]
         -- | Workig directory of launched application.
         , appWorkDir :: Maybe FilePath
         -- | The applications environment variables.
         , appEnvironment :: Maybe (M.Map T.Text T.Text)
         -- | List of isolation steps that SHOULD be applied to the app.
         , appIsolators :: Maybe [Isolator]
         -- | Locations where this app expects external data to be mounted.
         , appMntPoints :: Maybe [MountPoint]
         -- | Ports where this app will be listening on.
         , appPorts :: Maybe [Port]
         }
  deriving (Eq, Show)

data EventHandler =
       EventHandler
         {
         -- | Command to run.
         ehExec :: [T.Text]
         -- | When command is to be executed, pre-start/post-stop.
         , ehName :: EventHandlerName
         }
  deriving (Eq, Show)

data EventHandlerName = PreStart
                      | PostStop
  deriving (Eq, Show)

data Isolator =
       Isolator
         {
         -- | Name of the isolator to apply.
         isoName :: ACIdentifier
         -- | Value to apply.
         , isoValue :: Value
         }
  deriving (Eq, Show)

data MountPoint =
       MountPoint
         {
         -- | Mount point name.
         mpName :: T.Text
         -- | Path inside the rootfs.
         , mpPath :: FilePath
         -- | Whether th mountpoint is read-only.
         , mpReadOnly :: Bool
         }
  deriving (Eq, Show)

data ACDependency =
       ACDependency
         {
         -- | Name of the dependent App Container image.
         depImageName :: ACIdentifier
         -- | Content hash of the dependency.
         , depImageID :: Maybe T.Text
         -- | List of labels for the dependency.
         , depLabels :: Maybe (M.Map ACIdentifier T.Text)
         -- | The size of the dependency in bytes.
         , depSize :: Maybe Integer
         }
  deriving (Eq, Show)

data Port =
       Port
         {
         -- | Descriptive name for this port.
         portName :: T.Text
         -- | Protocol that will be used on this port.
         , portProto :: T.Text
         -- | Port number that will be used.
         , portNum :: T.Text
         -- | A range of ports starting with portNum and ending with portNum + count -1.
         , portCount :: Maybe Int
         -- | If true the application expectes to be socket activated on these ^ ports. NOTE : MUST default
         -- to False if unsupplied.
         , portSocketActivated :: Bool
         }
  deriving (Eq, Show)

data ACAnnotation =
       ACAnnotation
         {
         -- | Name of the annotation.
         annoName :: T.Text
         -- | Value of the annotation.
         , annoValue :: T.Text
         }
  deriving (Eq, Show)

newtype ACKind = ACKind T.Text
  deriving (Eq, Show)

-- | AC identifier type.
newtype ACIdentifier = ACIdentifier T.Text
  deriving (Eq, Show)

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
    tailAllowed =
      do
        t <- takeWhile (inClass "-a-z0-9._/~")
        let endc = T.last t
        if not (isLetter endc || isNumber endc)
          then fail ("Expected to end with a character or a digit, found " ++
                     [endc])
          else return t
    toAcid ic s = ACIdentifier $ T.cons ic s
