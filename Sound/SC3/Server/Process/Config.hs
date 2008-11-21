-- | Read server options from configuraton file.
module Sound.SC3.Server.Process.Config (
    fromAssocs,
    toAssocs
) where

import Control.Monad.State              (State, evalState, execState)
import Data.Accessor
import Sound.OpenSoundControl           (TCP, UDP)
import Sound.SC3.Server.Process.Options
import Data.ConfigFile                  (OptionSpec)
import Data.Map                         (Map)
import qualified Data.Map               as Map

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
             [(a, "")] -> return a
             _         -> fail ("Could not read " ++ (show s))

set :: (Read b) => Map OptionSpec String -> OptionSpec -> Accessor a b -> State a ()
set opts name accessor = do
    case Map.lookup name opts of
        Nothing  -> return ()
        Just str -> case readMaybe str of
                        Nothing    -> return ()
                        Just value -> putA accessor value

get :: (Show b) => OptionSpec -> Accessor a b -> State a (OptionSpec, String)
get name accessor = do
    value <- getA accessor
    return (name, show value)

-- | Get 'ServerOptions' from an option 'Map'.
-- Uninitialized fields are taken from 'defaultServerOptions'.
getServerOptions :: Map OptionSpec String -> ServerOptions
getServerOptions m = flip execState defaultServerOptions $ do
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
getRTOptions :: Map OptionSpec String -> RTOptions
getRTOptions m = flip execState defaultRTOptions $ do
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
getNRTOptions :: Map OptionSpec String -> NRTOptions
getNRTOptions m = flip execState defaultNRTOptions $ do
    set m "commandFilePath"            _commandFilePath
    set m "inputFilePath"              _inputFilePath
    set m "outputFilePath"             _outputFilePath
    set m "outputSampleRate"           _outputSampleRate
    set m "outputHeaderFormat"         _outputHeaderFormat
    set m "outputSampleFormat"         _outputSampleFormat

-- | Read server options, realtime options and non-relatime options from an
-- association list.
fromAssocs :: [(OptionSpec, String)] -> (ServerOptions, RTOptions, NRTOptions)
fromAssocs opts = (getServerOptions m, getRTOptions m, getNRTOptions m)
    where m = Map.fromList opts

-- | Convert 'ServerOptions' to association list.
assocsServerOptions :: ServerOptions -> [(OptionSpec, String)]
assocsServerOptions options = flip evalState options $ sequence [
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
assocsRTOptions :: RTOptions -> [(OptionSpec, String)]
assocsRTOptions options = flip evalState options $ sequence [
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
assocsNRTOptions :: NRTOptions -> [(OptionSpec, String)]
assocsNRTOptions options = flip evalState options $ sequence [
      get "commandFilePath"            _commandFilePath
    , get "inputFilePath"              _inputFilePath
    , get "outputFilePath"             _outputFilePath
    , get "outputSampleRate"           _outputSampleRate
    , get "outputHeaderFormat"         _outputHeaderFormat
    , get "outputSampleFormat"         _outputSampleFormat
    ]

-- | Convert server options and optionally realtime options and non-realtime
-- options to an association list.
toAssocs :: ServerOptions -> Maybe RTOptions -> Maybe NRTOptions -> [(OptionSpec, String)]
toAssocs serverOptions rtOptions nrtOptions =
    assocsServerOptions serverOptions
    ++ maybe [] assocsRTOptions rtOptions
    ++ maybe [] assocsNRTOptions nrtOptions
