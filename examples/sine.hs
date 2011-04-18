import Control.Concurrent
import Sound.OpenSoundControl (OSC(..), Time(..), UDP, pauseThread)
import Sound.SC3
import Sound.SC3.Server.Process
import System.Exit

sine :: UGen
sine = out 0 (mce2 (s 390) (s 400))
    where s f = sinOsc AR f 0.1 * 0.3

scmain :: UDP -> IO ()
scmain fd = do
    reset fd
    play fd sine
    pauseThread 10
    send fd quit

main :: IO ()   
main = withSynth
        openUDP
        (defaultServerOptions
            { serverProgram = "scsynth"
            , loadSynthDefs = False })
        (defaultRTOptionsUDP
            { udpPortNumber = 2278 })
        defaultOutputHandler
        scmain
