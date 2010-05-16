module Control.Concurrent.Mailbox.Wrapper
    ( Wrappable (..)

    , wrapReadHandle
    , wrapWriteHandle
    , wrapReadWriteHandle
    , wrapReadHandleWithMailbox
    , wrapWriteHandleWithMailbox
    )
where

import Control.Concurrent.Mailbox

import Control.Concurrent
import System.IO

type ErrorHandler m = Mailbox m -> IOError -> IO ()
type Wrapper m = (Handle -> Mailbox m -> ErrorHandler m -> IO ())

class Wrappable m where
    toString :: m -> String

    fromString :: String -> Maybe m
    fromString str =
        case fromStringReadS str of
            [(msg, "")] -> Just msg
            _           -> Nothing

    fromStringReadS :: ReadS m
    fromStringReadS str =
        case fromString str of
            Just msg -> [(msg, "")]
            Nothing  -> []

wrapReadHandle
    :: Wrappable m
    => Handle
    -> ErrorHandler m
    -> IO (Mailbox m, ThreadId)
wrapReadHandle = wrapHandle inWrapper

wrapWriteHandle
    :: Wrappable m
    => Handle
    -> ErrorHandler m
    -> IO (Mailbox m, ThreadId)
wrapWriteHandle = wrapHandle outWrapper

-- | Maps both in and out wrapper to the hdl
-- and return both Mailbox and ThreadId 
wrapReadWriteHandle 
    :: Wrappable m 
    => Handle 
    -> IO (Mailbox m, ThreadId, Mailbox m, ThreadId)
wrapReadWriteHandle hdl = do
    (i,t1)  <- wrapHandle inWrapper hdl
    (o,t2)  <- wrapHandle outWrapper hdl
    return (i,t1,o,t2)

wrapHandle
    :: Wrappable m
    => Wrapper m
    -> Handle
    -> ErrorHandler m
    -> IO (Mailbox m, ThreadId)
wrapHandle wrapper hdl errHandler = do
    mbox <- newMailbox
    tid <- forkIO $ wrapper hdl mbox errHandler
    return (mbox, tid)

wrapReadHandleWithMailbox
    :: Wrappable m
    => Handle
    -> Mailbox m
    -> ErrorHandler m
    -> IO ThreadId
wrapReadHandleWithMailbox = wrapHandleWithMailbox inWrapper

wrapWriteHandleWithMailbox
    :: Wrappable m
    => Handle
    -> Mailbox m
    -> ErrorHandler m
    -> IO ThreadId
wrapWriteHandleWithMailbox = wrapHandleWithMailbox outWrapper

wrapHandleWithMailbox
    :: Wrappable m
    => Wrapper m
    -> Handle
    -> Mailbox m
    -> ErrorHandler m
    -> IO ThreadId
wrapHandleWithMailbox wrapper hdl mbox errHandler =
    forkIO $ wrapper hdl mbox errHandler

inWrapper
    :: Wrappable m
    => Wrapper m
inWrapper hdl mbox errHandler = do
    eline <- catch (fmap Left $ hGetLine hdl) (return . Right)

    case eline of
        Left line -> do
            case fromString line of
                Just msg -> mbox ! msg
                Nothing  -> putStrLn $ "Error: Cannot parse message: " ++ show line

            inWrapper hdl mbox errHandler
        Right e ->
            errHandler mbox e

outWrapper
    :: Wrappable m
    => Wrapper m
outWrapper hdl mbox errHandler = do
    receive mbox
        [ \msg -> (#) $ do
            let smsg = toString msg

            me <- catch (do
                hPutStr hdl smsg
                hPutChar hdl '\n'
                hFlush hdl
                return Nothing)
                (return . Just)

            case me of
                Nothing -> outWrapper hdl mbox errHandler
                Just e  -> errHandler mbox e
        ]

