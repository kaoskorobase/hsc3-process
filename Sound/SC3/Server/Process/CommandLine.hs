{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TypeSynonymInstances #-}
module Sound.SC3.Server.Process.CommandLine (
    rtCommandLine
  , nrtCommandLine
) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Sound.SC3.Server.Enum
import Sound.SC3.Server.Process.Options

-- ====================================================================
-- scsynth commandline options

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

instance Option [FilePath] where
    showOption = intercalate ":"

instance Option SoundFileFormat where
    showOption = soundFileFormatString

instance Option SampleFormat where
    showOption = sampleFormatString

data ToOption a = forall b . Option b => ToOption (a -> b)

toOption :: a -> ToOption a -> String
toOption a (ToOption f) = showOption (f a)

mkOption :: a -> a -> String -> ToOption a -> Maybe (String, String)
mkOption defaultOptions options optName accessor =
        if value == defaultValue
        then Nothing
        else Just (optName, value)
    where
        defaultValue = defaultOptions `toOption` accessor
        value        = options `toOption` accessor

mkOptions :: a -> a -> [(String, ToOption a)] -> [(String, String)]
mkOptions defaultOptions options assocs = [x | Just x <- map (uncurry $ mkOption defaultOptions options) assocs]

flattenOptions :: [(a, a)] -> [a]
flattenOptions [] = []
flattenOptions ((a, b):xs) = a : b : flattenOptions xs

mkServerOptions :: ServerOptions -> [String]
mkServerOptions options = (flattenOptions.mkOptions defaultServerOptions options) [ 
    ("-c" , ToOption numberOfControlBusChannels)
  , ("-a" , ToOption numberOfAudioBusChannels  )
  , ("-i" , ToOption numberOfInputBusChannels  )
  , ("-o" , ToOption numberOfOutputBusChannels )
  , ("-z" , ToOption blockSize                 )
  , ("-b" , ToOption numberOfSampleBuffers     )
  , ("-n" , ToOption maxNumberOfNodes          )
  , ("-d" , ToOption maxNumberOfSynthDefs      )
  , ("-m" , ToOption realtimeMemorySize        )
  , ("-w" , ToOption numberOfWireBuffers       )
  , ("-r" , ToOption numberOfRandomSeeds       )
  , ("-D" , ToOption loadSynthDefs             )
  , ("-v" , ToOption verbosity                 )
  , ("-U" , ToOption ugenPluginPath            )
  , ("-P" , ToOption restrictedPath            ) ]

mkRTOptions :: RTOptions -> [String]
mkRTOptions options =
    (case networkPort options of
        UDPPort p -> ["-u", showOption p]
        TCPPort p -> ["-t", showOption p])
    ++
    (flattenOptions $ mkOptions defaultRTOptions options $
        [ ("-R" , ToOption useZeroconf          )
        , ("-H" , ToOption hardwareDeviceName   )
        , ("-Z" , ToOption hardwareBufferSize   )
        , ("-S" , ToOption hardwareSampleRate   )
        , ("-l" , ToOption maxNumberOfLogins    )
        , ("-p" , ToOption sessionPassword      )
        , ("-I" , ToOption inputStreamsEnabled  )
        , ("-O" , ToOption outputStreamsEnabled ) ])

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
