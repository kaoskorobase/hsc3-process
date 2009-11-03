-- | Read server options from configuraton file.
module Sound.SC3.Server.Process.ConfigFile (
    fromAssocs,
    toAssocs
) where
    
import Control.Monad.Error              (MonadError)
import Control.Monad.Trans.State        (StateT, evalStateT, execStateT)
import Data.Accessor
import Sound.OpenSoundControl           (TCP, UDP)
import Sound.SC3.Server.Process.Options
import Data.ConfigFile                  (CPError, OptionSpec)
import Data.Map                         (Map)
import qualified Data.Map               as Map

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
             [(a, "")] -> return a
             _         -> fail ("Could not read " ++ (show s))

-- TODO: Add parse error handling
set :: (Read b, Monad m) => Map OptionSpec String -> OptionSpec -> Accessor a b -> StateT a m ()
set opts name accessor = do
    case Map.lookup name opts of
        Nothing  -> return ()
        Just str -> case readMaybe str of
                        Nothing    -> return ()
                        Just value -> putA accessor value

get :: (Show b, Monad m) => OptionSpec -> Accessor a b -> StateT a m (OptionSpec, String)
get name accessor = do
    value <- getA accessor
    return (name, show value)

-- | Get 'ServerOptions' from an option 'Map'.
-- Uninitialized fields are taken from 'defaultServerOptions'.
getServerOptions :: (Monad m) => Map OptionSpec String -> m ServerOptions
getServerOptions m = flip execStateT defaultServerOptions $ do
    set m "serverProgram"              _serverProgram
    set m "numberOfControlBusChannels" _numberOfControlBusChannels
    set m "numberOfAudioBusChannels"   _numberOfAudioBusChannels
    set m "numberOfInputBusChannels"   _numberOfInputBusChannels
    set m "numberOfOutputBusChannels"  _numberOfOutputBusChannels
    set m "blockSize"                  _blockSize
    set m "numberOfSampleBuffers"      _numberOfSampleBuffers
    set m "maxNumberOfNodes"           _maxNumberOfNodes
    set m "maxNumberOfSynthDefs"       _maxNumberOfSynthDefs
    set m "realtimeMemorySize"         _realtimeMemorySize
    set m "numberOfWireBuffers"        _numberOfWireBuffers
    set m "numberOfRandomSeeds"        _numberOfRandomSeeds
    set m "loadSynthDefs"              _loadSynthDefs
    set m "verbosity"                  _verbosity

-- | Get 'RTOptions' from an option 'Map'.
-- Uninitialized fields are taken from 'defaultRTOptions'.
getRTOptions :: (Monad m) => Map OptionSpec String -> m RTOptions
getRTOptions m = flip execStateT defaultRTOptions $ do
    set m "udpPortNumber"              _udpPortNumber
    set m "tcpPortNumber"              _tcpPortNumber
    set m "useZeroconf"                _useZeroconf
    set m "maxNumberOfLogins"          _maxNumberOfLogins
    set m "sessionPassword"            _sessionPassword
    set m "hardwareDeviceName"         _hardwareDeviceName
    set m "hardwareBufferSize"         _hardwareBufferSize
    set m "hardwareSampleRate"         _hardwareSampleRate
    set m "inputStreamsEnabled"        _inputStreamsEnabled
    set m "outputStreamsEnabled"       _outputStreamsEnabled

-- | Get 'NRTOptions' from an option 'Map'.
-- Uninitialized fields are taken from 'defaultNRTOptions'.
getNRTOptions :: Monad m => Map OptionSpec String -> m NRTOptions
getNRTOptions m = flip execStateT defaultNRTOptions $ do
    set m "commandFilePath"            _commandFilePath
    set m "inputFilePath"              _inputFilePath
    set m "outputFilePath"             _outputFilePath
    set m "outputSampleRate"           _outputSampleRate
    set m "outputHeaderFormat"         _outputHeaderFormat
    set m "outputSampleFormat"         _outputSampleFormat

-- | Read server options, realtime options and non-relatime options from an
-- association list.
--
-- TODO: Add error handling.
fromAssocs :: MonadError CPError m => [(OptionSpec, String)] -> m (ServerOptions, RTOptions, NRTOptions)
fromAssocs opts = do
    srvo <- getServerOptions m
    rto  <- getRTOptions m
    nrto <- getNRTOptions m
    return (srvo, rto, nrto)
    where m = Map.fromList opts

-- | Convert 'ServerOptions' to association list.
assocsServerOptions :: Monad m => ServerOptions -> m [(OptionSpec, String)]
assocsServerOptions options = flip evalStateT options $ sequence [
      get "serverProgram"              _serverProgram
    , get "numberOfControlBusChannels" _numberOfControlBusChannels
    , get "numberOfAudioBusChannels"   _numberOfAudioBusChannels
    , get "numberOfInputBusChannels"   _numberOfInputBusChannels
    , get "numberOfOutputBusChannels"  _numberOfOutputBusChannels
    , get "blockSize"                  _blockSize
    , get "numberOfSampleBuffers"      _numberOfSampleBuffers
    , get "maxNumberOfNodes"           _maxNumberOfNodes
    , get "maxNumberOfSynthDefs"       _maxNumberOfSynthDefs
    , get "realtimeMemorySize"         _realtimeMemorySize
    , get "numberOfWireBuffers"        _numberOfWireBuffers
    , get "numberOfRandomSeeds"        _numberOfRandomSeeds
    , get "loadSynthDefs"              _loadSynthDefs
    , get "verbosity"                  _verbosity
    ]

-- | Convert 'RTOptions' to association list.
assocsRTOptions :: Monad m => RTOptions -> m [(OptionSpec, String)]
assocsRTOptions options = flip evalStateT options $ sequence [
      get "udpPortNumber"              _udpPortNumber
    , get "tcpPortNumber"              _tcpPortNumber
    , get "useZeroconf"                _useZeroconf
    , get "maxNumberOfLogins"          _maxNumberOfLogins
    , get "sessionPassword"            _sessionPassword
    , get "hardwareDeviceName"         _hardwareDeviceName
    , get "hardwareBufferSize"         _hardwareBufferSize
    , get "hardwareSampleRate"         _hardwareSampleRate
    , get "inputStreamsEnabled"        _inputStreamsEnabled
    , get "outputStreamsEnabled"       _outputStreamsEnabled
    ]

-- | Convert 'NRTOptions' to association list.
assocsNRTOptions :: Monad m => NRTOptions -> m [(OptionSpec, String)]
assocsNRTOptions options = flip evalStateT options $ sequence [
      get "commandFilePath"            _commandFilePath
    , get "inputFilePath"              _inputFilePath
    , get "outputFilePath"             _outputFilePath
    , get "outputSampleRate"           _outputSampleRate
    , get "outputHeaderFormat"         _outputHeaderFormat
    , get "outputSampleFormat"         _outputSampleFormat
    ]

-- | Convert server options and optionally realtime options and non-realtime
-- options to an association list.
toAssocs :: Monad m => ServerOptions -> Maybe RTOptions -> Maybe NRTOptions -> m [(OptionSpec, String)]
toAssocs serverOptions rtOptions nrtOptions = do
    srvo <- assocsServerOptions serverOptions
    rto  <- maybe (return []) assocsRTOptions rtOptions
    nrto <- maybe (return []) assocsNRTOptions nrtOptions
    return $ srvo++rto++nrto
