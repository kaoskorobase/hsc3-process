{-# LANGUAGE FlexibleInstances #-}
module Sound.SC3.Server.Process.CommandLine (
    rtCommandLine
  , nrtCommandLine
) where

import Data.Default (Default, def)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Sound.SC3.Server.Enum
import Sound.SC3.Server.Process.Options

-- ====================================================================
-- scsynth commandline options

-- | Convert a value to an option string.
class Show a => Option a where
    showOption :: a -> String

instance Option String where
    showOption = id

instance Option Int where
    showOption = show

instance Option Bool where
    showOption = showOption . fromEnum

instance Option a => Option (Maybe a) where
    showOption Nothing  = ""
    showOption (Just a) = showOption a

instance Option Verbosity where
    showOption = showOption . fromEnum

instance Option [FilePath] where
    showOption = intercalate ":"

instance Option NetworkPort where
  showOption (UDPPort p) = show p
  showOption (TCPPort p) = show p

instance Option SoundFileFormat where
    showOption = soundFileFormatString

instance Option SampleFormat where
    showOption = sampleFormatString

option :: (Default a, Option b, Eq b) => a -> String -> (a -> b) -> Maybe (String, String)
option options flag accessor =
      if value == accessor def
      then Nothing
      else Just (flag, showOption value)
    where
      value = accessor options

flatten :: [Maybe (a, a)] -> [a]
flatten [] = []
flatten (Nothing:xs) = flatten xs
flatten (Just (a, b):xs) = a : b : flatten xs

mkServerOptions :: ServerOptions -> [String]
mkServerOptions options = flatten [
    option options "-c" numberOfControlBusChannels
  , option options "-a" numberOfAudioBusChannels
  , option options "-i" numberOfInputBusChannels
  , option options "-o" numberOfOutputBusChannels
  , option options "-z" blockSize
  , option options "-b" numberOfSampleBuffers
  , option options "-n" maxNumberOfNodes
  , option options "-d" maxNumberOfSynthDefs
  , option options "-m" realtimeMemorySize
  , option options "-w" numberOfWireBuffers
  , option options "-r" numberOfRandomSeeds
  , option options "-D" loadSynthDefs
  , option options "-v" verbosity
  , option options "-U" ugenPluginPath
  , option options "-P" restrictedPath ]

mkRTOptions :: RTOptions -> [String]
mkRTOptions options = flatten $
    [ case networkPort options of
        UDPPort _ -> option options "-u" networkPort
        TCPPort _ -> option options "-t" networkPort ]
    ++
    [ option options "-R" useZeroconf
    , option options "-H" hardwareDeviceName
    , option options "-Z" hardwareBufferSize
    , option options "-S" hardwareSampleRate
    , option options "-l" maxNumberOfLogins
    , option options "-p" sessionPassword
    , option options "-I" inputStreamsEnabled
    , option options "-O" outputStreamsEnabled ]

mkNRTOptions :: NRTOptions -> Maybe FilePath -> [String]
mkNRTOptions options commandFilePath =
    "-N" : [
          fromMaybe "_" commandFilePath
        , fromMaybe "_" (inputFilePath options)
        , outputFilePath options
        , showOption (outputSampleRate options)
        , showOption (outputSoundFileFormat options)
        , showOption (outputSampleFormat options) ]

-- | Construct the scsynth command line from 'ServerOptions' and 'RTOptions'.
rtCommandLine :: ServerOptions -> RTOptions -> [String]
rtCommandLine serverOptions rtOptions =
    (serverProgram serverOptions : mkServerOptions serverOptions)
 ++ mkRTOptions rtOptions

-- | Construct the scsynth command line from 'ServerOptions' and 'NRTOptions'.
nrtCommandLine :: ServerOptions -> NRTOptions -> Maybe FilePath -> [String]
nrtCommandLine serverOptions nrtOptions commandFilePath =
    (serverProgram serverOptions : mkServerOptions serverOptions)
 ++ mkNRTOptions nrtOptions commandFilePath
