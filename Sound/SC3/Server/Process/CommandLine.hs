module Sound.SC3.Server.Process.CommandLine (
    CommandLine(..),
    commandLine
) where

import Data.Accessor
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Sound.SC3.Server.Process.Options

-- ====================================================================
-- scsynth commandline options

class CommandLine a where
    argumentList :: a -> [String]

-- | Convert a value to an option string.
class Show a => Option a where
    showOption :: a -> String

instance Option (String) where
    showOption = id

instance Option (Int) where
    showOption = show

instance Option (Bool) where
    showOption = showOption . fromEnum

instance Option a => Option (Maybe a) where
    showOption Nothing  = ""
    showOption (Just a) = showOption a

instance Option (Verbosity) where
    showOption = showOption . ((-)2) . fromEnum

instance Option [FilePath] where
    showOption = intercalate ":" . map show

mkOption :: (Eq b, Option b, Show b) => a -> a -> String -> Accessor a b -> [String]
mkOption defaultOptions options optName accessor =
        if value == defaultValue
        then []
        else [optName, showOption value]
    where
        defaultValue = defaultOptions ^. accessor
        value        = options ^. accessor

instance CommandLine (ServerOptions) where
    argumentList options = concat [ 
              o "-c" _numberOfControlBusChannels
            , o "-a" _numberOfAudioBusChannels
            , o "-i" _numberOfInputBusChannels
            , o "-o" _numberOfOutputBusChannels
            , o "-z" _blockSize
            , o "-b" _numberOfSampleBuffers
            , o "-n" _maxNumberOfNodes
            , o "-d" _maxNumberOfSynthDefs
			, o "-m" _realtimeMemorySize
            , o "-w" _numberOfWireBuffers
            , o "-r" _numberOfRandomSeeds
            , o "-D" _loadSynthDefs
            , o "-v" _verbosity
            , o "-U" _ugenPluginPath
            , o "-P" _restrictedPath ]
        where
            o :: (Eq b, Option b, Show b) => String -> Accessor ServerOptions b -> [String]
            o = mkOption defaultServerOptions options

instance CommandLine (RTOptions) where
    argumentList options = concat [
              o "-u" _udpPortNumber
            , o "-t" _tcpPortNumber
            , o "-R" _useZeroconf
            , o "-H" _hardwareDeviceName
            , o "-Z" _hardwareBufferSize
            , o "-S" _hardwareSampleRate
            , o "-l" _maxNumberOfLogins
            , o "-p" _sessionPassword
            , o "-I" _inputStreamsEnabled
            , o "-O" _outputStreamsEnabled ]
        where
            o :: (Eq b, Option b, Show b) => String -> Accessor RTOptions b -> [String]
            o = mkOption defaultRTOptions options

instance CommandLine (NRTOptions) where
    argumentList options =
        "-N" : map ($ options) [
              fromMaybe "_" . commandFilePath
            , fromMaybe "_" . inputFilePath
            , outputFilePath
            , showOption . outputSampleRate
            , outputHeaderFormat
            , outputSampleFormat ]

-- | Construct the scsynth command line from 'ServerOptions' and either 'RTOptions' or 'NRTOptions'.
commandLine :: (CommandLine a) => ServerOptions -> a -> [String]
commandLine serverOptions options = (serverProgram serverOptions : argumentList serverOptions) ++ argumentList options
