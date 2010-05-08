module Control.Concurrent.Mailbox.Wrapper
    ( Wrappable (..)

    , wrapReadHandle
    , wrapWriteHandle
    , wrapReadHandleWithMailbox
    , wrapWriteHandleWithMailbox
    )
where

import Control.Concurrent.Mailbox

import Control.Concurrent
import System.IO
import System.IO.Error

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
    -> IO (Mailbox m, ThreadId)
wrapReadHandle = wrapHandle inWrapper

wrapWriteHandle
    :: Wrappable m
    => Handle
    -> IO (Mailbox m, ThreadId)
wrapWriteHandle = wrapHandle outWrapper

wrapHandle
    :: Wrappable m
    => (Handle -> Mailbox m -> IO ())
    -> Handle
    -> IO (Mailbox m, ThreadId)
wrapHandle wrapper hdl = do
    mbox <- newMailbox
    tid <- forkIO $ wrapper hdl mbox
    return (mbox, tid)

wrapReadHandleWithMailbox
    :: Wrappable m
    => Handle
    -> Mailbox m
    -> IO ThreadId
wrapReadHandleWithMailbox = wrapHandleWithMailbox inWrapper

wrapWriteHandleWithMailbox
    :: Wrappable m
    => Handle
    -> Mailbox m
    -> IO ThreadId
wrapWriteHandleWithMailbox = wrapHandleWithMailbox outWrapper

wrapHandleWithMailbox
    :: Wrappable m
    => (Handle -> Mailbox m -> IO ())
    -> Handle
    -> Mailbox m
    -> IO ThreadId
wrapHandleWithMailbox wrapper hdl mbox = forkIO $ wrapper hdl mbox

inWrapper
    :: Wrappable m
    => Handle
    -> Mailbox m
    -> IO ()
inWrapper hdl mbox = do
    eline <- catch (fmap Left $ hGetLine hdl) (return . Right)

    case eline of
        Left line -> do
            case fromString line of
                Just msg -> mbox ! msg
                Nothing  -> putStrLn $ "Error: Cannot parse message: " ++ show line

            inWrapper hdl mbox
        Right e -> do
            mbox ! (error $ show e)
            print $ ioeGetErrorType e

outWrapper
    :: Wrappable m
    => Handle
    -> Mailbox m
    -> IO ()
outWrapper hdl mbox = do
    receive mbox
        [ \msg -> (#) $ do
            hPutStr hdl $ toString msg
            hPutChar hdl '\n'
            hFlush hdl
        ]

    outWrapper hdl mbox

