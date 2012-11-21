import Control.Monad (forM_)
import Sound.OpenSoundControl (bundle)
import Sound.OSC.FD (Time(NTPr))
import Sound.SC3.FD
import Sound.SC3.Server.Process

sine :: UGen
sine = out 0 (mce2 (s (freq*0.99)) (s (freq*1.01)))
  where s f = sinOsc AR f 0.1 * 0.3
        freq = control KR "freq" 440

score :: Double -> NRT
score freq = NRT $ map (\(t, ms) -> bundle (NTPr t) ms) $ [
    (0,  [ d_recv (synthdef "sine" sine) ])
  , (0,  [ s_new "sine" 1 AddToTail 0 [ ("freq", freq) ] ])
  , (10, [ n_free [ 1 ] ])
  ]

main :: IO ()
main = forM_ [200,400,800] $ \freq ->
  withNRT
    defaultServerOptions
    (defaultNRTOptions { outputFilePath = "output_" ++ show freq ++ ".wav" })
    defaultOutputHandler
    (flip putNRT (score freq))
