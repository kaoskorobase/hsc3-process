module Sound.SC3.Server.Transport (
    OpenTransport(..)
) where

import Sound.OpenSoundControl (Transport)
import Sound.SC3.Server.Options

-- | Helper class for polymorphic opening of network connections.
class Transport t => OpenTransport t where
    -- | Open a transport to scsynth based on the given options and an optional hostname.
    openTransport :: ServerOptions -> RTOptions -> Maybe String -> IO t
