import Sound.OpenSoundControl   (UDP)
import Sound.SC3
import Sound.SC3.Server.Process
import Data.Accessor

scmain :: UDP -> IO ()
scmain _ = return ()

main :: IO ()   
main = do
    putStrLn "fuck"
    withSynth defaultServerOptions defaultRTOptionsUDP ((onBoot^=scmain) defaultEventHandler)
    return ()
