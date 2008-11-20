module Sound.SC3.Server.Process.Options where

import Data.Accessor
import Data.Accessor.Template (deriveAccessors)

-- | Used with the 'verbosity' field in 'ServerOptions'.
data Verbosity =
    Silent
  | Quiet
  | Normal
  | Verbose
  | VeryVerbose
  | ExtremelyVerbose
  deriving (Eq, Read, Show)

-- 'Enum' instance for 'Verbosity' for conversion to a commandline option.
instance Enum (Verbosity) where
    fromEnum Silent             = -2
    fromEnum Quiet              = -1
    fromEnum Normal             =  0
    fromEnum Verbose            =  1
    fromEnum VeryVerbose        =  2
    fromEnum ExtremelyVerbose   =  4

    toEnum (-1)                 = Quiet
    toEnum 0                    = Normal
    toEnum 1                    = Verbose
    toEnum 2                    = VeryVerbose
    toEnum x | x >= 4           = ExtremelyVerbose
    toEnum _                    = Silent

-- ====================================================================
-- * Server options

-- | Specify general server options used both in realtime and non-realtime
-- mode.
data ServerOptions = ServerOptions {
    serverProgram_               :: FilePath,    -- ^ Path to the @scsynth@ program
    numberOfControlBusChannels_  :: Int,         -- ^ Number of allocated control bus channels
    numberOfAudioBusChannels_    :: Int,         -- ^ Number of allocated audio bus channels
    numberOfInputBusChannels_    :: Int,         -- ^ Number of physical input channels
    numberOfOutputBusChannels_   :: Int,         -- ^ Number of physical output channels
    blockSize_                   :: Int,         -- ^ Synthesis block size
    numberOfSampleBuffers_       :: Int,         -- ^ Number of allocated sample buffers
    maxNumberOfNodes_            :: Int,         -- ^ Maximum number of synthesis nodes
    maxNumberOfSynthDefs_        :: Int,         -- ^ Maximum number of synth definitions
    realTimeMemorySize_          :: Int,         -- ^ Realtime memory size in bytes
    numberOfWireBuffers_         :: Int,         -- ^ Number of unit generator connection buffers
    numberOfRandomSeeds_         :: Int,         -- ^ Number of random number generator seeds
    loadSynthDefs_               :: Bool,        -- ^ If 'True', load synth definitions from /synthdefs/ directory on startup
    verbosity_                   :: Verbosity   -- ^ 'Verbosity' level
} deriving (Eq, Show)

-- | Default server options.
defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions {
    serverProgram_              = "scsynth",
    numberOfControlBusChannels_ = 4096,
    numberOfAudioBusChannels_   = 128,
    numberOfInputBusChannels_   = 8,
    numberOfOutputBusChannels_  = 8,
    blockSize_                  = 64,
    numberOfSampleBuffers_      = 1024,
    maxNumberOfNodes_           = 1024,
    maxNumberOfSynthDefs_       = 1024,
    realTimeMemorySize_         = 8192,
    numberOfWireBuffers_        = 64,
    numberOfRandomSeeds_        = 64,
    loadSynthDefs_              = True,
    verbosity_                  = Normal
}

$(deriveAccessors ''ServerOptions)

-- | Realtime server options, parameterized by the OpenSoundControl
-- 'Transport' to be used.
data RTOptions = RTOptions {
    -- Network control
    udpPortNumber_           :: Int,             -- ^ UDP port number (one of 'udpPortNumber' and 'tcpPortNumber' must be non-zero)
    tcpPortNumber_           :: Int,             -- ^ TCP port number (one of 'udpPortNumber' and 'tcpPortNumber' must be non-zero)
    useZeroconf_             :: Bool,            -- ^ If 'True', publish scsynth service through Zeroconf
    maxNumberOfLogins_       :: Int,             -- ^ Max number of supported logins if 'sessionPassword' is set
    sessionPassword_         :: Maybe String,    -- ^ Session password
    -- Audio device control
    hardwareDeviceName_      :: Maybe String,    -- ^ Hardware device name (JACK client:server name on Linux)
    hardwareBufferSize_      :: Int,             -- ^ Hardware buffer size (no effect with JACK)
    hardwareSampleRate_      :: Int,             -- ^ Hardware buffer size (no effect with JACK)
    inputStreamsEnabled_     :: Maybe Int,       -- ^ Enabled input streams (CoreAudio only)
    outputStreamsEnabled_    :: Maybe Int        -- ^ Enabled output streams (CoreAudio only)
} deriving (Eq, Show)

-- | Default realtime server options.
defaultRTOptions :: RTOptions
defaultRTOptions = RTOptions {
    -- Network control
    udpPortNumber_           = 0,
    tcpPortNumber_           = 0,
    useZeroconf_             = False,
    maxNumberOfLogins_       = 16,
    sessionPassword_         = Nothing,
    -- Audio device control
    hardwareDeviceName_      = Nothing,
    hardwareBufferSize_      = 0,
    hardwareSampleRate_      = 0,
    inputStreamsEnabled_     = Nothing,
    outputStreamsEnabled_    = Nothing
}

$(deriveAccessors ''RTOptions)

-- | Default realtime server options (UDP transport).
defaultRTOptionsUDP :: RTOptions
defaultRTOptionsUDP =  flip ($) defaultRTOptions (udpPortNumber ^: const 57110)

-- | Default realtime server options (TCP transport).
defaultRTOptionsTCP :: RTOptions
defaultRTOptionsTCP = flip ($) defaultRTOptions (tcpPortNumber ^: const 57110)

-- ====================================================================
-- * Non-Realtime options

-- | Non-realtime server options.
data NRTOptions = NRTOptions {
    commandFilePath_    :: Maybe FilePath,  -- ^ Path to OSC command file ('Nothing' for stdin)
    inputFilePath_      :: Maybe FilePath,  -- ^ Path to input sound file ('Nothing' for no audio input)
    outputFilePath_     :: FilePath,        -- ^ Path to output sound file
    outputSampleRate_   :: Int,             -- ^ Output sound file sample rate
    outputHeaderFormat_ :: String,          -- ^ Output sound file header format
    outputSampleFormat_ :: String           -- ^ Output sound file sample format
} deriving (Eq, Show)

-- | Default non-realtime server options.
defaultNRTOptions :: NRTOptions
defaultNRTOptions = NRTOptions {
    commandFilePath_         = Nothing,
    inputFilePath_           = Nothing,
    outputFilePath_          = "output.wav",
    outputSampleRate_        = 44100,
    outputHeaderFormat_      = "wav",
    outputSampleFormat_      = "int16"
}

$(deriveAccessors ''NRTOptions)

