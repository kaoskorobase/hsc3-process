{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.SC3.Server.Internal (
    InternalTransport
) where

import           Bindings.Sound.SC3
import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.StablePtr
import           Sound.OpenSoundControl as OSC
import qualified Sound.SC3 as SC
import           Sound.SC3.Server.Options
import           Sound.SC3.Server.Transport

type World = Ptr C'World
type ReplyAddress = Ptr C'ReplyAddress
type ReplyFunc = ReplyAddress -> Ptr CChar -> CInt -> IO ()

foreign import ccall "wrapper"
    mkReplyFunc :: ReplyFunc -> IO (FunPtr ReplyFunc)

data InternalTransport = InternalTransport {
    world :: World
  , recvChan :: Chan OSC
  }

openIT :: Ptr C'WorldOptions -> IO InternalTransport
openIT options = do
    -- w <- alloca $ \ptr -> do
    --         ptr `poke` options
    --         c'World_New ptr
    w <- c'World_New options
    c <- newChan
    return $ InternalTransport w c

it_replyFunc :: ReplyAddress -> Ptr CChar -> CInt -> IO ()
it_replyFunc replyAddress cbuf csize = do
    -- buf <- BS.create (fromIntegral size) $ \ptr ->
    --     copyBytes ptr (castPtr cbuf) size
    buf <- BS.packCStringLen (cbuf, fromIntegral csize)
    ptr <- liftM castPtrToStablePtr (c'ReplyAddress_ReplyData replyAddress)
    (f, t) <- deRefStablePtr ptr
    let osc = decodeOSC (BL.fromChunks [buf])
    -- putStrLn $ "it_replyFunc: " ++ show osc
    writeChan (recvChan t) osc
    freeHaskellFunPtr f
    freeStablePtr ptr

copyChunks :: Ptr CChar -> [BS.ByteString] -> IO ()
copyChunks dst = foldM_ f 0
    where
        f i b = do
            let (fp, o, n) = BS.toForeignPtr b
            withForeignPtr fp $ \src ->
                copyBytes (dst `plusPtr` i)
                          (src `plusPtr` o)
                          n
            return (i + n)

copyByteString :: Ptr CChar -> BL.ByteString -> IO ()
copyByteString dst = copyChunks dst . BL.toChunks

sendIT :: InternalTransport -> OSC -> IO ()
sendIT t osc = do
    let buf = encodeOSC osc
        n = BL.length buf
    replyFunc <- mkReplyFunc it_replyFunc
    replyFuncData <- newStablePtr (replyFunc, t)
    allocaArray (fromIntegral n) $ \cbuf -> do
        copyByteString cbuf buf
        c'World_SendPacketWithContext
            (world t)
            (fromIntegral n)
            cbuf
            replyFunc
            (castStablePtrToPtr replyFuncData)
    return ()

recvIT :: InternalTransport -> IO OSC
recvIT = readChan . recvChan

closeIT :: InternalTransport -> IO ()
closeIT t = do
    sendIT t SC.quit
    c'World_WaitForQuit (world t)

withWorldOptions :: (Ptr C'WorldOptions -> IO a) -> ServerOptions -> RTOptions -> IO a
withWorldOptions f so ro = do
    (fs, cs) <- flip execStateT ([], []) $ do
        -- c'WorldOptions'mPassword :: CString
        setOpt (\x -> x { c'WorldOptions'mNumBuffers = int (numberOfSampleBuffers so) })
        setOpt (\x -> x { c'WorldOptions'mMaxLogins  = int (maxNumberOfLogins ro) })
        setOpt (\x -> x { c'WorldOptions'mMaxNodes   = int (maxNumberOfNodes so) })
        setOpt (\x -> x { c'WorldOptions'mMaxGraphDefs = int (maxNumberOfSynthDefs so) })
        setOpt (\x -> x { c'WorldOptions'mMaxWireBufs = int (numberOfWireBuffers so) })
        setOpt (\x -> x { c'WorldOptions'mNumAudioBusChannels = int (numberOfAudioBusChannels so) })
        setOpt (\x -> x { c'WorldOptions'mNumInputBusChannels = int (numberOfInputBusChannels so) })
        setOpt (\x -> x { c'WorldOptions'mNumOutputBusChannels = int (numberOfOutputBusChannels so) })
        setOpt (\x -> x { c'WorldOptions'mNumControlBusChannels = int (numberOfControlBusChannels so) })
        setOpt (\x -> x { c'WorldOptions'mBufLength = int (blockSize so) })
        setOpt (\x -> x { c'WorldOptions'mRealTimeMemorySize = int (realtimeMemorySize so) })
        -- TODO: Make shared controls accessible
        setOpt (\x -> x { c'WorldOptions'mNumSharedControls = 0 })
        setOpt (\x -> x { c'WorldOptions'mSharedControls = nullPtr })
        -- TODO
        -- tell (\x -> x { c'WorldOptions'mRealTime })
        -- TODO
        -- tell (\x -> x { c'WorldOptions'mMemoryLocking :: CInt
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeCmdFilename :: CString
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeInputFilename :: CString
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeOutputFilename :: CString
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeOutputHeaderFormat :: CString
        -- tell (\x -> x { c'WorldOptions'mNonRealTimeOutputSampleFormat :: CString
        setOpt (\x -> x { c'WorldOptions'mPreferredSampleRate = int (hardwareSampleRate ro) })
        setOpt (\x -> x { c'WorldOptions'mNumRGens = int (numberOfRandomSeeds so) })
        setOpt (\x -> x { c'WorldOptions'mPreferredHardwareBufferFrameSize = int (hardwareBufferSize ro) })
        setOpt (\x -> x { c'WorldOptions'mLoadGraphDefs = bool (loadSynthDefs so) })
        -- tell (\x -> x { c'WorldOptions'mInputStreamsEnabled :: CString
        -- tell (\x -> x { c'WorldOptions'mOutputStreamsEnabled :: CString
        -- TODO: Make input and output device configurable separately
        case hardwareDeviceName ro of
            Nothing -> return ()
            Just name -> do
                setOptS (\x s -> x { c'WorldOptions'mInDeviceName = s }) name
                setOptS (\x s -> x { c'WorldOptions'mOutDeviceName = s }) name
        setOpt (\x -> x { c'WorldOptions'mVerbosity = int (fromEnum (verbosity so)) })
        setOpt (\x -> x { c'WorldOptions'mRendezvous = bool (useZeroconf ro) })
        maybe (return ()) (setOptS (\x s -> x { c'WorldOptions'mUGensPluginPath = s }) . path) (ugenPluginPath so)
        maybe (return ()) (setOptS (\x s -> x { c'WorldOptions'mRestrictedPath = s })) (restrictedPath so)
    opts <- liftM (flip (foldl (flip ($))) fs) (c'kDefaultWorldOptions >>= peek)
    a <- alloca $ \ptr -> do
        ptr `poke` opts
        f ptr
    mapM_ free cs
    return a
    where
        int :: (Integral a, Num b) => a -> b
        int = fromIntegral
        bool :: Num a => Bool -> a
        bool b = int (if b then 1 else 0)
        path :: [String] -> String
        path = L.intercalate ":"
        setOpt f = do
            (fs, cs) <- get
            put (f:fs, cs)
        setOptS f s = do
            ptr <- liftIO (newCString s)
            (fs, cs) <- get
            put (flip f ptr:fs, ptr:cs)
            
instance OSC.Transport InternalTransport where
    send  = sendIT
    recv  = recvIT
    close = closeIT

instance OpenTransport InternalTransport where
    openTransport so ro _ = withWorldOptions openIT so ro

-- main = do
--     opts <- c'kDefaultWorldOptions >>= peek
--     print opts
--     t <- newIT opts
--     send t (SC.notify True)
--     recv t >>= print
--     putStrLn "sending status ..."
--     send t (SC.status)
--     recv t >>= print
--     close t
    -- world <- alloca $ \opts -> do
    --             opts `poke` defaultWorldOptions
    --             c'World_New opts
    -- c'World_Cleanup world
