module Sound.SC3.Server.Process.Options
  ( Verbosity(..)
  , ServerOptions(..)
  , defaultServerOptions
  , fromBuildDirectory
  , fromPrefix
  , fromApplicationBundle
  , NetworkPort(..)
  , defaultUDPPort
  , defaultTCPPort
  , RTOptions(..)
  , onPort
  , jackDeviceName
  , defaultRTOptions
  , defaultRTOptionsUDP
  , defaultRTOptionsTCP
  , NRTOptions(..)
  , defaultNRTOptions
  ) where

import Data.Default (Default(..))
import Sound.SC3.Server.Enum
import System.FilePath ((</>))

-- | Used with the 'verbosity' field in 'ServerOptions'.
data Verbosity =
    Silent
  | Quiet
  | Normal
  | Verbose
  | VeryVerbose
  | ExtremelyVerbose
  deriving (Eq, Read, Show)

instance Enum Verbosity where
    fromEnum Silent           = -2
    fromEnum Quiet            = -1
    fromEnum Normal           = 0
    fromEnum Verbose          = 1
    fromEnum VeryVerbose      = 2
    fromEnum ExtremelyVerbose = 3
    toEnum (-2)               = Silent
    toEnum (-1)               = Quiet
    toEnum 0                  = Normal
    toEnum 1                  = Verbose
    toEnum 2                  = VeryVerbose
    toEnum 3                  = ExtremelyVerbose
    toEnum _                  = error "Verbosity (toEnum): bad argument"

instance Default Verbosity where
  def = Normal

-- ====================================================================
-- * Server options

-- | Specify general server options used both in realtime and non-realtime
-- mode.
data ServerOptions = ServerOptions {
    serverProgram               :: FilePath          -- ^ Path to the @scsynth@ program
  , numberOfControlBusChannels  :: Int               -- ^ Number of allocated control bus channels
  , numberOfAudioBusChannels    :: Int               -- ^ Number of allocated audio bus channels
  , numberOfInputBusChannels    :: Int               -- ^ Number of physical input channels
  , numberOfOutputBusChannels   :: Int               -- ^ Number of physical output channels
  , blockSize                   :: Int               -- ^ Synthesis block size
  , numberOfSampleBuffers       :: Int               -- ^ Number of allocated sample buffers
  , maxNumberOfNodes            :: Int               -- ^ Maximum number of synthesis nodes
  , maxNumberOfSynthDefs        :: Int               -- ^ Maximum number of synth definitions
  , realtimeMemorySize          :: Int               -- ^ Realtime memory size in bytes
  , numberOfWireBuffers         :: Int               -- ^ Number of unit generator connection buffers
  , numberOfRandomSeeds         :: Int               -- ^ Number of random number generator seeds
  , loadSynthDefs               :: Bool              -- ^ If 'True', load synth definitions from /synthdefs/ directory on startup
  , verbosity                   :: Verbosity         -- ^ 'Verbosity' level
  , ugenPluginPath              :: Maybe [FilePath]  -- ^ List of UGen plugin search paths
  , restrictedPath              :: Maybe FilePath    -- ^ Sandbox path to restrict OSC command filesystem access
  } deriving (Eq, Show)

instance Default ServerOptions where
  def = defaultServerOptions

-- | Default server options.
defaultServerOptions :: ServerOptions
{-# DEPRECATED defaultServerOptions "Use Data.Default.Default instance instead" #-}
defaultServerOptions = ServerOptions {
    serverProgram              = "scsynth"
  , numberOfControlBusChannels = 4096
  , numberOfAudioBusChannels   = 128
  , numberOfInputBusChannels   = 8
  , numberOfOutputBusChannels  = 8
  , blockSize                  = 64
  , numberOfSampleBuffers      = 1024
  , maxNumberOfNodes           = 1024
  , maxNumberOfSynthDefs       = 1024
  , realtimeMemorySize         = 8192
  , numberOfWireBuffers        = 64
  , numberOfRandomSeeds        = 64
  , loadSynthDefs              = True
  , verbosity                  = def
  , ugenPluginPath             = Nothing
  , restrictedPath             = Nothing
  }

-- | Run @scsynth@ from a SuperCollider build directory.
--
-- Since 0.8.0
fromBuildDirectory :: FilePath -> ServerOptions -> ServerOptions
fromBuildDirectory dir options = options {
    serverProgram = dir </> "server/scsynth/scsynth"
  , ugenPluginPath = Just [ dir </> "server/plugins" ]
  }

-- | Run @scsynth@ from a Linux installation prefix.
--
-- Since 0.8.0
fromPrefix :: FilePath -> ServerOptions -> ServerOptions
fromPrefix dir options = options {
    serverProgram = dir </> "bin/scsynth"
  , ugenPluginPath = Just [ dir </> "lib/SuperCollider/plugins" ]
  }

-- | Run @scsynth@ from an OSX application bundle.
--
-- Since 0.8.0
fromApplicationBundle :: FilePath -> ServerOptions -> ServerOptions
fromApplicationBundle dir options = options {
    serverProgram = resources </> "scsynth"
  , ugenPluginPath = Just [ resources </> "plugins" ]
  }
  where resources = dir </> "Contents/Resources"

-- ====================================================================
-- * Realtime options

-- | Network port.
data NetworkPort =
    UDPPort Int
  | TCPPort Int
  deriving (Eq, Show)

instance Default NetworkPort where
  def = defaultUDPPort

-- | Default network port number.
defaultPortNumber :: Int
defaultPortNumber = 57110

-- | Default UDP port.
defaultUDPPort :: NetworkPort
defaultUDPPort = UDPPort defaultPortNumber

-- | Default TCP port.
defaultTCPPort :: NetworkPort
defaultTCPPort = TCPPort defaultPortNumber

-- | Realtime server options, parameterized by the OpenSoundControl
-- 'Transport' to be used.
data RTOptions = RTOptions {
    -- Network control
    networkPort             :: NetworkPort     -- ^ Network port
  , useZeroconf             :: Bool            -- ^ If 'True', publish scsynth service through Zeroconf
  , maxNumberOfLogins       :: Int             -- ^ Max number of supported logins if 'sessionPassword' is set
  , sessionPassword         :: Maybe String    -- ^ Session password
  -- Audio device control
  , hardwareDeviceName      :: Maybe String    -- ^ Hardware device name (see also 'jackDeviceName')
  , hardwareBufferSize      :: Int             -- ^ Hardware buffer size (no effect with JACK)
  , hardwareSampleRate      :: Int             -- ^ Hardware buffer size (no effect with JACK)
  , inputStreamsEnabled     :: Maybe Int       -- ^ Enabled input streams (CoreAudio only)
  , outputStreamsEnabled    :: Maybe Int       -- ^ Enabled output streams (CoreAudio only)
  } deriving (Eq, Show)

instance Default RTOptions where
  def = defaultRTOptions

-- | Default realtime server options.
defaultRTOptions :: RTOptions
{-# DEPRECATED defaultRTOptions "Use Data.Default.Default instance instead" #-}
defaultRTOptions = RTOptions {
    -- Network control
    networkPort             = def
  , useZeroconf             = False
  , maxNumberOfLogins       = 16
  , sessionPassword         = Nothing
    -- Audio device control
  , hardwareDeviceName      = Nothing
  , hardwareBufferSize      = 0
  , hardwareSampleRate      = 0
  , inputStreamsEnabled     = Nothing
  , outputStreamsEnabled    = Nothing
  }

-- | Create RTOptions with a specific network port.
--
-- Since 0.8.0
onPort :: NetworkPort -> RTOptions
onPort port = def { networkPort = port }

-- | Create a JACK hardware device name from an optional server name and a
--   client name.
--
-- Since 0.8.0
jackDeviceName :: Maybe String -> String -> String
jackDeviceName = (++) . maybe "" ((++":"))

-- | Default realtime server options (UDP transport).
defaultRTOptionsUDP :: RTOptions
{-# DEPRECATED defaultRTOptionsUDP "Use 'onPort defaultUDPPort' instead" #-}
defaultRTOptionsUDP =  defaultRTOptions { networkPort = defaultUDPPort }

-- | Default realtime server options (TCP transport).
defaultRTOptionsTCP :: RTOptions
{-# DEPRECATED defaultRTOptionsTCP "Use 'onPort defaultTCPPort' instead" #-}
defaultRTOptionsTCP = defaultRTOptions { networkPort = defaultTCPPort }

-- ====================================================================
-- * Non-Realtime options

-- | Non-realtime server options.
data NRTOptions = NRTOptions {
    inputFilePath         :: Maybe FilePath    -- ^ Path to input sound file ('Nothing' for no audio input)
  , outputFilePath        :: FilePath          -- ^ Path to output sound file
  , outputSampleRate      :: Int               -- ^ Output sound file sample rate
  , outputSoundFileFormat :: SoundFileFormat   -- ^ Output sound file header format (since 0.8.0)
  , outputSampleFormat    :: SampleFormat      -- ^ Output sound file sample format
  } deriving (Eq, Show)

instance Default NRTOptions where
  def = defaultNRTOptions

-- | Default non-realtime server options.
defaultNRTOptions :: NRTOptions
{-# DEPRECATED defaultNRTOptions "Use Data.Default.Default instance instead" #-}
defaultNRTOptions = NRTOptions {
    inputFilePath         = Nothing
  , outputFilePath        = "output.wav"
  , outputSampleRate      = 44100
  , outputSoundFileFormat = Wave
  , outputSampleFormat    = PcmInt16
  }
