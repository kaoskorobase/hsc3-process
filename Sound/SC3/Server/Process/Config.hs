-- | Read server options from file
module Sound.SC3.Server.Process.Config (
    fromSection
) where

import Data.Accessor
import Sound.OpenSoundControl           (TCP, UDP)
import Sound.SC3.Server.Process.Options
import Data.ConfigFile                  (OptionSpec)
import Data.Map                         (Map)
import qualified Data.Map               as Map

import Control.Monad.State      (State, execState)

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

getNRTOptions :: Map OptionSpec String -> NRTOptions
getNRTOptions m = flip execState defaultNRTOptions $ do
    set m "commandFilePath"            _commandFilePath
    set m "inputFilePath"              _inputFilePath
    set m "outputFilePath"             _outputFilePath
    set m "outputSampleRate"           _outputSampleRate
    set m "outputHeaderFormat"         _outputHeaderFormat
    set m "outputSampleFormat"         _outputSampleFormat

fromSection :: [(OptionSpec, String)] -> (ServerOptions, RTOptions, NRTOptions)
fromSection opts = (getServerOptions m, getRTOptions m, getNRTOptions m)
    where m = Map.fromList opts

-- setServerOptions options = flip execState options $ sequence [
--       get "serverProgram"              serverProgram
--     , get "numberOfControlBusChannels" numberOfControlBusChannels
--     , get "numberOfAudioBusChannels"   numberOfAudioBusChannels
--     , get "numberOfInputBusChannels"   numberOfInputBusChannels
--     , get "numberOfOutputBusChannels"  numberOfOutputBusChannels
--     , get "blockSize"                  blockSize
--     , get "numberOfSampleBuffers"      numberOfSampleBuffers
--     , get "maxNumberOfNodes"           maxNumberOfNodes
--     , get "maxNumberOfSynthDefs"       maxNumberOfSynthDefs
--     , get "realTimeMemorySize"         realTimeMemorySize
--     , get "numberOfWireBuffers"        numberOfWireBuffers
--     , get "numberOfRandomSeeds"        numberOfRandomSeeds
--     , get "loadSynthDefs"              loadSynthDefs
--     , get "verbosity"                  verbosity
--     ]


-- toSection :: ServerOptions -> Maybe RTOptions -> Maybe NRTOption -> [(OptionSpec, String)]
-- toSection = undefined

-- fromSectionTCP :: [(OptionSpec, String)] -> (ServerOptions, RTOptions TCP)
-- fromSectionTCP opts = (getServerOptions m, getRTOptionsTCP m)
--     where m = Map.fromList opts
-- 
-- fromSectionNRT :: [(OptionSpec, String)] -> (ServerOptions, NRTOptions)
-- fromSectionNRT opts = (getServerOptions m, getNRTOptions m)
--     where m = Map.fromList opts

-- -- ServerOptions
-- set_serverProgram              x v = x { serverProgram              = v }
-- set_numberOfControlBusChannels x v = x { numberOfControlBusChannels = v }
-- set_numberOfAudioBusChannels   x v = x { numberOfAudioBusChannels   = v }
-- set_numberOfInputBusChannels   x v = x { numberOfInputBusChannels   = v }
-- set_numberOfOutputBusChannels  x v = x { numberOfOutputBusChannels  = v }
-- set_blockSize                  x v = x { blockSize                  = v }
-- set_numberOfSampleBuffers      x v = x { numberOfSampleBuffers      = v }
-- set_maxNumberOfNodes           x v = x { maxNumberOfNodes           = v }
-- set_maxNumberOfSynthDefs       x v = x { maxNumberOfSynthDefs       = v }
-- set_realTimeMemorySize         x v = x { realTimeMemorySize         = v }
-- set_numberOfWireBuffers        x v = x { numberOfWireBuffers        = v }
-- set_numberOfRandomSeeds        x v = x { numberOfRandomSeeds        = v }
-- set_loadSynthDefs              x v = x { loadSynthDefs              = v }
-- set_verbosity                  x v = x { verbosity                  = v }
-- 
-- -- RTOptions
-- set_udpPortNumber              x v = x { udpPortNumber        = v }
-- set_tcpPortNumber              x v = x { tcpPortNumber        = v }
-- set_useZeroconf                x v = x { useZeroconf          = v }
-- set_maxNumberOfLogins          x v = x { maxNumberOfLogins    = v }
-- set_sessionPassword            x v = x { sessionPassword      = v }
-- set_hardwareDeviceName         x v = x { hardwareDeviceName   = v }
-- set_hardwareBufferSize         x v = x { hardwareBufferSize   = v }
-- set_hardwareSampleRate         x v = x { hardwareSampleRate   = v }
-- set_inputStreamsEnabled        x v = x { inputStreamsEnabled  = v }
-- set_outputStreamsEnabled       x v = x { outputStreamsEnabled = v }
-- 
-- -- NRTOptions
-- set_commandFilePath            x v = x { commandFilePath    = v }
-- set_inputFilePath              x v = x { inputFilePath      = v }
-- set_outputFilePath             x v = x { outputFilePath     = v }
-- set_outputSampleRate           x v = x { outputSampleRate   = v }
-- set_outputHeaderFormat         x v = x { outputHeaderFormat = v }
-- set_outputSampleFormat         x v = x { outputSampleFormat = v }
