{-# LANGUAGE FlexibleContexts #-}
-- | Read server options from configuraton file.
module Sound.SC3.Server.Process.ConfigFile (
    getOptions
  , setOptions
) where

import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import           Data.Accessor
import qualified Data.Accessor.Monad.Trans.State as A
import qualified Data.List as L
import           Sound.SC3.Server.Options
import           Data.ConfigFile
import           Text.Regex

newtype MaybeConfig a = MaybeConfig { maybeConfig :: Maybe a }

instance Show a => Show (MaybeConfig a) where
    show = maybe "" show . maybeConfig

instance Get_C a => Get_C (MaybeConfig a) where
    get parser section option = liftM (MaybeConfig . Just) (get parser section option)

data FilePathList = FilePathList { filePathList :: [FilePath] }

instance Show FilePathList where
    show = L.intercalate ":" . filePathList

instance Get_C FilePathList where
    get parser section option = liftM (FilePathList . splitRegex r) (get parser section option)
        where r = mkRegex ":"

getField :: (MonadError CPError m, Get_C b) => ConfigParser -> SectionSpec -> OptionSpec -> Accessor a b -> StateT a m ()
getField parser section option accessor = do
    case get parser section option of
        Left _  -> return ()
        Right x -> A.set accessor x

getField_ :: (MonadError CPError m, Get_C c) => ConfigParser -> SectionSpec -> (c -> b) -> OptionSpec -> Accessor a b -> StateT a m ()
getField_ parser section wrapper option accessor = do
    case get parser section option of
        Left _ -> return ()
        Right x -> A.set accessor (wrapper x)

-- | Get 'ServerOptions' from an option 'Map'.
-- Uninitialized fields are taken from 'defaultServerOptions'.
getServerOptions :: (MonadError CPError m) => ConfigParser -> SectionSpec -> m ServerOptions
getServerOptions parser section = flip State.execStateT defaultServerOptions $ do
    getField parser section                        "serverProgram"              _serverProgram
    getField parser section                        "numberOfControlBusChannels" _numberOfControlBusChannels
    getField parser section                        "numberOfAudioBusChannels"   _numberOfAudioBusChannels
    getField parser section                        "numberOfInputBusChannels"   _numberOfInputBusChannels
    getField parser section                        "numberOfOutputBusChannels"  _numberOfOutputBusChannels
    getField parser section                        "blockSize"                  _blockSize
    getField parser section                        "numberOfSampleBuffers"      _numberOfSampleBuffers
    getField parser section                        "maxNumberOfNodes"           _maxNumberOfNodes
    getField parser section                        "maxNumberOfSynthDefs"       _maxNumberOfSynthDefs
    getField parser section                        "realtimeMemorySize"         _realtimeMemorySize
    getField parser section                        "numberOfWireBuffers"        _numberOfWireBuffers
    getField parser section                        "numberOfRandomSeeds"        _numberOfRandomSeeds
    getField parser section                        "loadSynthDefs"              _loadSynthDefs
    getField parser section                        "verbosity"                  _verbosity
    getField_ parser section (Just . filePathList) "ugenPluginPath"             _ugenPluginPath
    getField_ parser section maybeConfig           "restrictedPath"             _restrictedPath

-- | Get 'RTOptions' from an option 'Map'.
-- Uninitialized fields are taken from 'defaultRTOptions'.
getRTOptions :: (MonadError CPError m) => ConfigParser -> SectionSpec -> m RTOptions
getRTOptions parser section = flip State.execStateT defaultRTOptions $ do
    getField parser section              "udpPortNumber"        _udpPortNumber
    getField parser section              "tcpPortNumber"        _tcpPortNumber
    getField parser section              "useZeroconf"          _useZeroconf
    getField parser section              "maxNumberOfLogins"    _maxNumberOfLogins
    getField_ parser section maybeConfig "sessionPassword"      _sessionPassword
    getField_ parser section maybeConfig "hardwareDeviceName"   _hardwareDeviceName
    getField parser section              "hardwareBufferSize"   _hardwareBufferSize
    getField parser section              "hardwareSampleRate"   _hardwareSampleRate
    getField_ parser section maybeConfig "inputStreamsEnabled"  _inputStreamsEnabled
    getField_ parser section maybeConfig "outputStreamsEnabled" _outputStreamsEnabled

-- | Get 'NRTOptions' from an option 'Map'.
-- Uninitialized fields are taken from 'defaultNRTOptions'.
getNRTOptions :: MonadError CPError m => ConfigParser -> SectionSpec -> m NRTOptions
getNRTOptions parser section = flip State.execStateT defaultNRTOptions $ do
    getField_ parser section maybeConfig "commandFilePath"    _commandFilePath
    getField_ parser section maybeConfig "inputFilePath"      _inputFilePath
    getField parser section              "outputFilePath"     _outputFilePath
    getField parser section              "outputSampleRate"   _outputSampleRate
    getField parser section              "outputHeaderFormat" _outputHeaderFormat
    getField parser section              "outputSampleFormat" _outputSampleFormat

-- | Read server options, realtime options and non-relatime options from a 'ConfigParser'.
getOptions :: MonadError CPError m => ConfigParser -> SectionSpec -> m (ServerOptions, RTOptions, NRTOptions)
getOptions parser section = do
    so <- getServerOptions parser section
    ro <- getRTOptions parser section
    no <- getNRTOptions parser section
    return (so, ro, no)

setField :: (Show b, MonadError CPError m) => SectionSpec -> OptionSpec -> Accessor a b -> StateT (a, ConfigParser) m ()
setField section option accessor = do
    (options, parser) <- State.get
    let value = getVal accessor options
    parser' <- lift (setshow parser section option value)
    State.put (options, parser')

setField_ :: (Show c, MonadError CPError m) => (b -> c) -> SectionSpec -> OptionSpec -> Accessor a b -> StateT (a, ConfigParser) m ()
setField_ wrapper section option accessor = do
    (options, parser) <- State.get
    let value = getVal accessor options
    parser' <- lift (setshow parser section option (wrapper value))
    State.put (options, parser')

-- -- | Convert 'ServerOptions' to association list.
setServerOptions :: MonadError CPError m => ServerOptions -> ConfigParser -> SectionSpec -> m ConfigParser
setServerOptions options parser section = liftM snd $ flip State.execStateT (options, parser) $ do
    setField                                    section "serverProgram"              _serverProgram
    setField                                    section "numberOfControlBusChannels" _numberOfControlBusChannels
    setField                                    section "numberOfAudioBusChannels"   _numberOfAudioBusChannels
    setField                                    section "numberOfInputBusChannels"   _numberOfInputBusChannels
    setField                                    section "numberOfOutputBusChannels"  _numberOfOutputBusChannels
    setField                                    section "blockSize"                  _blockSize
    setField                                    section "numberOfSampleBuffers"      _numberOfSampleBuffers
    setField                                    section "maxNumberOfNodes"           _maxNumberOfNodes
    setField                                    section "maxNumberOfSynthDefs"       _maxNumberOfSynthDefs
    setField                                    section "realtimeMemorySize"         _realtimeMemorySize
    setField                                    section "numberOfWireBuffers"        _numberOfWireBuffers
    setField                                    section "numberOfRandomSeeds"        _numberOfRandomSeeds
    setField                                    section "loadSynthDefs"              _loadSynthDefs
    setField                                    section "verbosity"                  _verbosity
    setField_ (MaybeConfig . fmap FilePathList) section "ugenPluginPath"             _ugenPluginPath
    setField_ MaybeConfig                       section "restrictedPath"            _restrictedPath

-- -- | Convert 'RTOptions' to association list.
setRTOptions :: MonadError CPError m => RTOptions -> ConfigParser -> SectionSpec -> m ConfigParser
setRTOptions options parser section = liftM snd $ flip State.execStateT (options, parser) $ do
    setField              section "udpPortNumber"        _udpPortNumber
    setField              section "tcpPortNumber"        _tcpPortNumber
    setField              section "useZeroconf"          _useZeroconf
    setField              section "maxNumberOfLogins"    _maxNumberOfLogins
    setField_ MaybeConfig section "sessionPassword"      _sessionPassword
    setField_ MaybeConfig section "hardwareDeviceName"   _hardwareDeviceName
    setField              section "hardwareBufferSize"   _hardwareBufferSize
    setField              section "hardwareSampleRate"   _hardwareSampleRate
    setField_ MaybeConfig section "inputStreamsEnabled"  _inputStreamsEnabled
    setField_ MaybeConfig section "outputStreamsEnabled" _outputStreamsEnabled

-- -- | Convert 'NRTOptions' to association list.
setNRTOptions :: MonadError CPError m => NRTOptions -> ConfigParser -> SectionSpec -> m ConfigParser
setNRTOptions options parser section = liftM snd $ flip State.execStateT (options, parser) $ do
    setField_ MaybeConfig section "commandFilePath"    _commandFilePath
    setField_ MaybeConfig section "inputFilePath"      _inputFilePath
    setField              section "outputFilePath"     _outputFilePath
    setField              section "outputSampleRate"   _outputSampleRate
    setField              section "outputHeaderFormat" _outputHeaderFormat
    setField              section "outputSampleFormat" _outputSampleFormat


-- | Convert server options and optionally realtime options and non-realtime
-- options to an association list.
setOptions :: MonadError CPError m => ConfigParser -> SectionSpec -> (ServerOptions, RTOptions, NRTOptions) -> m ConfigParser
setOptions p0 section (so, ro, no) = do
    p1 <- setServerOptions so p0 section
    p2 <- setRTOptions     ro p1 section
    p3 <- setNRTOptions    no p2 section
    return p3
