{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TypeSynonymInstances #-}
module Sound.SC3.Server.Process.CommandLine (
    rtCommandLine
  , nrtCommandLine
  , mkOption
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
    showOption = intercalate ":"

data ToOption a = forall b . Option b => ToOption (a -> b)

toOption :: a -> ToOption a -> String
toOption a (ToOption f) = showOption (f a)

mkOption_ :: (Eq b, Option b, Show b) => a -> a -> String -> Accessor a b -> [String]
mkOption_ defaultOptions options optName accessor =
        if value == defaultValue
        then []
        else [optName, showOption value]
    where
        defaultValue = defaultOptions ^. accessor
        value        = options ^. accessor

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
mkRTOptions options = (flattenOptions.mkOptions defaultRTOptions options) [
    ("-u" , ToOption udpPortNumber        )
  , ("-t" , ToOption tcpPortNumber        )
  , ("-R" , ToOption useZeroconf          )
  , ("-H" , ToOption hardwareDeviceName   )
  , ("-Z" , ToOption hardwareBufferSize   )
  , ("-S" , ToOption hardwareSampleRate   )
  , ("-l" , ToOption maxNumberOfLogins    )
  , ("-p" , ToOption sessionPassword      )
  , ("-I" , ToOption inputStreamsEnabled  )
  , ("-O" , ToOption outputStreamsEnabled ) ]

mkNRTOptions :: NRTOptions -> [String]
mkNRTOptions options =
    "-N" : map ($ options) [
          fromMaybe "_" . commandFilePath
        , fromMaybe "_" . inputFilePath
        , outputFilePath
        , showOption . outputSampleRate
        , outputHeaderFormat
        , outputSampleFormat ]

-- | Construct the scsynth command line from 'ServerOptions' and 'RTOptions'.
rtCommandLine :: ServerOptions -> RTOptions -> [String]
rtCommandLine serverOptions rtOptions = (serverProgram serverOptions : mkServerOptions serverOptions) ++ mkRTOptions rtOptions

-- | Construct the scsynth command line from 'ServerOptions' and 'NRTOptions'.
nrtCommandLine :: ServerOptions -> NRTOptions -> [String]
nrtCommandLine serverOptions nrtOptions = (serverProgram serverOptions : mkServerOptions serverOptions) ++ mkNRTOptions nrtOptions
