module Sound.SC3.Server.Process.CommandLine (
    CommandLine(..),
    commandLine
) where

import Data.Accessor
import Data.Maybe                       (fromMaybe)
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
    showOption = showOption . fromEnum

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
              o "-c" numberOfControlBusChannels
            , o "-a" numberOfAudioBusChannels
            , o "-i" numberOfInputBusChannels
            , o "-o" numberOfOutputBusChannels
            , o "-z" blockSize
            , o "-b" numberOfSampleBuffers
            , o "-n" maxNumberOfNodes
            , o "-d" maxNumberOfSynthDefs
            , o "-w" numberOfWireBuffers
            , o "-r" numberOfRandomSeeds
            , o "-D" loadSynthDefs
            , o "-v" verbosity ]
        where
            o :: (Eq b, Option b, Show b) => String -> Accessor ServerOptions b -> [String]
            o = mkOption defaultServerOptions options

instance CommandLine (RTOptions) where
    argumentList options = concat [
              o "-u" udpPortNumber
            , o "-t" tcpPortNumber
            , o "-R" useZeroconf
            , o "-H" hardwareDeviceName
            , o "-Z" hardwareBufferSize
            , o "-S" hardwareSampleRate
            , o "-l" maxNumberOfLogins
            , o "-p" sessionPassword
            , o "-I" inputStreamsEnabled
            , o "-O" outputStreamsEnabled ]
        where
            o :: (Eq b, Option b, Show b) => String -> Accessor RTOptions b -> [String]
            o = mkOption defaultRTOptions options

instance CommandLine (NRTOptions) where
    argumentList options =
        "-N" : map ($ options) [
              fromMaybe "_" . getVal commandFilePath
            , fromMaybe "_" . getVal inputFilePath
            , getVal outputFilePath
            , showOption . getVal outputSampleRate
            , getVal outputHeaderFormat
            , getVal outputSampleFormat ]

-- | Construct the scsynth command line from 'ServerOptions' and either 'RTOptions' or 'NRTOptions'.
commandLine :: (CommandLine a) => ServerOptions -> a -> [String]
commandLine serverOptions options = ((serverOptions ^. serverProgram) : argumentList serverOptions) ++ argumentList options
