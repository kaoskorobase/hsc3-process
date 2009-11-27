import Sound.OpenSoundControl (OSC(..), Time(..), UDP, pauseThread)
import Sound.SC3
import Sound.SC3.Server.Process
import Data.Accessor

sine :: UGen
sine = out 0 (mce2 (s 390) (s 400))
    where s f = sinOsc AR f 0.1 * 0.3

scmain :: UDP -> IO ()
scmain fd = do
    reset fd
    play fd sine
    pauseThread 4
    send fd quit

main :: IO ()   
main = do
    withSynth defaultServerOptions defaultRTOptionsUDP (defaultEventHandler { onBoot = scmain })
    return ()
