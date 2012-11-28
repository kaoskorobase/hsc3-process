module Sound.SC3.Server.Process.Make (
  renderNRT
, makeNRT
) where

import Control.Monad (when)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Compat (toUTCTime)
import Sound.SC3.Server.Process.CommandLine
import Sound.SC3.Server.Process.Options
import System.Directory
import System.Exit (ExitCode(ExitSuccess))
import System.Process (rawSystem)

-- | Render NRT @scsynth@ score via 'rawSystem', returning the process' exit code.
--
-- Since 0.8.0
renderNRT ::
    ServerOptions       -- ^ General server options
 -> NRTOptions          -- ^ Non-realtime server options
 -> FilePath            -- ^ NRT score file path
 -> IO ExitCode         -- ^ Process exit code
renderNRT serverOptions nrtOptions commandFilePath =
  let cf = Just commandFilePath
      exe:args = nrtCommandLine serverOptions nrtOptions cf
  in rawSystem exe args

-- | Variant of 'renderNRT' with simple modification-time
-- dependencies.  If the output file exists, and is more recent than
-- both the command file and the input audio file, rendering is not
-- performed.  It is an error if either the command file or the input
-- audio file, if specified, do not exist.
--
-- Since 0.8.0
makeNRT ::
    ServerOptions       -- ^ General server options
 -> NRTOptions          -- ^ Non-realtime server options
 -> FilePath            -- ^ NRT score file path
 -> IO ExitCode         -- ^ Process exit code
makeNRT serverOptions nrtOptions commandFilePath = do
  let o_fn = outputFilePath nrtOptions
      i_fn = inputFilePath nrtOptions
      epoch = UTCTime (fromGregorian 1858 11 17) 0
      get_mod_tm x fn = if x
                        then toUTCTime `fmap` getModificationTime fn
                        else return epoch
      up_to_date = print ("makeNRT: up to date",o_fn) >>
                   return ExitSuccess
      remake = print ("makeNRT: rendering",o_fn) >>
               renderNRT serverOptions nrtOptions commandFilePath
  c_x <- doesFileExist commandFilePath
  when (not c_x) (error "makeNRT: commandFilePath does not exist")
  i_x <- maybe (return True) doesFileExist i_fn
  when (not i_x) (error "makeNRT: inputFilePath does not exist")
  o_x <- doesFileExist o_fn
  c_t <- get_mod_tm c_x commandFilePath
  i_t <- maybe (return epoch) (get_mod_tm i_x) i_fn
  o_t <- get_mod_tm o_x o_fn
  if (o_t > c_t && o_t > i_t) then up_to_date else remake
