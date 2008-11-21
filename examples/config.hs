import Sound.SC3.Server.Process.ConfigFile  (fromAssocs)
import Data.ConfigFile
import System.Environment                   (getArgs)
import Control.Monad                        (join)
import Control.Monad.Error                  (ErrorT(..), liftIO)

main :: IO ()
main = do
    [file] <- getArgs
    runErrorT $ do
        cp <- join (liftIO (readfile emptyCP file))
        (serverOpts, rtOpts, nrtOpts) <- items cp "scsynth" >>= fromAssocs
        liftIO $ print serverOpts >> print rtOpts >> print nrtOpts
    return ()
