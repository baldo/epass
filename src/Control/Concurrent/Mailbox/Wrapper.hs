module Control.Concurrent.Mailbox.Wrapper
    ( Wrappable (..)
    , WrapBox

    , wrapReadHandle
    , wrapWriteHandle
    , wrapReadHandleWithMailbox
    , wrapWriteHandleWithMailbox

    , closeWrapper
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

data WrapBox m = WBox { mBox :: Mailbox m, tId :: ThreadId }

instance MailboxClass WrapBox where
    getMessage   = getMessage   . mBox
    unGetMessage = unGetMessage . mBox
    putMessage   = putMessage   . mBox
    isEmpty      = isEmpty      . mBox

wrapReadHandle
    :: Wrappable m
    => Handle
    -> ErrorHandler m
    -> IO (WrapBox m)
wrapReadHandle = wrapHandle inWrapper

wrapWriteHandle
    :: Wrappable m
    => Handle
    -> ErrorHandler m
    -> IO (WrapBox m)
wrapWriteHandle = wrapHandle outWrapper

wrapHandle
    :: Wrappable m
    => Wrapper m
    -> Handle
    -> ErrorHandler m
    -> IO (WrapBox m)
wrapHandle wrapper hdl errHandler = do
    mbox <- newMailbox
    tid <- forkIO $ wrapper hdl mbox errHandler
    return WBox { mBox = mbox, tId = tid }

wrapReadHandleWithMailbox
    :: Wrappable m
    => Handle
    -> Mailbox m
    -> ErrorHandler m
    -> IO (WrapBox m)
wrapReadHandleWithMailbox = wrapHandleWithMailbox inWrapper

wrapWriteHandleWithMailbox
    :: Wrappable m
    => Handle
    -> Mailbox m
    -> ErrorHandler m
    -> IO (WrapBox m)
wrapWriteHandleWithMailbox = wrapHandleWithMailbox outWrapper

wrapHandleWithMailbox
    :: Wrappable m
    => Wrapper m
    -> Handle
    -> Mailbox m
    -> ErrorHandler m
    -> IO (WrapBox m)
wrapHandleWithMailbox wrapper hdl mbox errHandler = do
    tid <- forkIO $ wrapper hdl mbox errHandler
    return WBox { mBox = mbox, tId = tid }

closeWrapper
    :: WrapBox m
    -> IO ()
closeWrapper = killThread . tId

inWrapper
    :: Wrappable m
    => Wrapper m
inWrapper hdl mbox errHandler = do
    eline <- catch (fmap Left $ hGetLine hdl) (return . Right)

    case eline of
        Left line -> do
            case fromString line of
                Just msg -> mbox <! msg
                Nothing  -> putStrLn $ "Error: Cannot parse message: " ++ show line

            inWrapper hdl mbox errHandler
        Right e ->
            errHandler mbox e

outWrapper
    :: Wrappable m
    => Wrapper m
outWrapper hdl mbox errHandler = do
    receive mbox
        [ \msg -> handler $ do
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

