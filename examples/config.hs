import Sound.SC3.Server.Process.Config  (fromSection)
import Data.ConfigFile
import System.Environment               (getArgs)
import Control.Monad                    (join)
import Control.Monad.Error              (ErrorT(..), liftIO)

main :: IO ()
main = do
    [file] <- getArgs
    runErrorT $ do
        cp   <- join (liftIO (readfile emptyCP file))
        opts <- items cp "scsynth"
        liftIO $ print (fromSection opts)
    return ()
    