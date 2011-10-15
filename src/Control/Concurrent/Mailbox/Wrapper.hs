-- | This module provides a wrapping mechanism for file handles (e.g. sockets)
module Control.Concurrent.Mailbox.Wrapper
    (
    -- * Wrapping types
      Wrappable (..)
    , WrapBox
    , ErrorHandler

    -- * Wrapping functions
    , wrapReadHandle
    , wrapWriteHandle

    , wrapReadHandleWithMailbox
    , wrapWriteHandleWithMailbox
    )
where

import Prelude hiding (catch)

import Control.Concurrent.Mailbox

import Control.Concurrent
import Control.Exception
import System.IO


-- Wrapping types --------------------------------------------------------------

{- | Messages send over wrapped handles must be instance of this class.

     You only need to implement either 'fromString' or 'fromStringReadS'.
-}
class Wrappable m where
    -- | Convert a message to a 'String'
    toString
        :: m      -- ^ the message
        -> String -- ^ its 'String' representation

    -- | Convert a 'String' to the represented message.
    fromString
        :: String  -- ^ 'String' representation of a message
        -> Maybe m -- ^ 'Just' the message or 'Nothing' in case of parse error
    fromString str =
        case fromStringReadS str of
            [(msg, "")] -> Just msg
            _           -> Nothing

    -- | Same as 'fromString' but using 'ReadS' type.
    fromStringReadS :: ReadS m
    fromStringReadS str =
        case fromString str of
            Just msg -> [(msg, "")]
            Nothing  -> []

{- | Wrapper around 'Mailbox'. For now the only 'MailboxClass' instance allowed
     for wrapping.
-}
data WrapBox m = WBox
        { mBox :: Mailbox m
        , tId  :: ThreadId
        }

instance MailboxClass WrapBox where
    getMessage   = getMessage   . mBox
    unGetMessage = unGetMessage . mBox
    putMessage   = putMessage   . mBox
    isEmpty      = isEmpty      . mBox

    close wbox = do
        killThread $ tId wbox
        close $ mBox wbox

{- | Function to be called in case of error. 'WrapBox' is the mailbox the error
     occured on.
-}
type ErrorHandler m = WrapBox m -> IOError -> IO ()


-- Wrapping functions ----------------------------------------------------------

{- | Wrap the given 'Handle' for reading. The returned 'WrapBox' can be used
     to receive messages from the 'Handle'.

     Notice: The 'ErrorHandler' will be given the returned 'WrapBox'. Writing to
     may not be what you want to do. Instead you might first call
     'wrapWriteHandle' and then use its 'WrapBox' in 'wrapReadHandle's
     'ErrorHandler'.
-}
wrapReadHandle
    :: Wrappable m
    => Handle         -- ^ handle to wrap
    -> ErrorHandler m -- ^ error handler
    -> IO (WrapBox m) -- ^ the wrapped mailbox
wrapReadHandle = wrapHandle inWrapper

{- | Wrap the given 'Handle' for writing. The returned 'WrapBox' can be used to
     send messages through the 'Handle'.
-}
wrapWriteHandle
    :: Wrappable m
    => Handle         -- ^ handle to wrap
    -> ErrorHandler m -- ^ error handler
    -> IO (WrapBox m) -- ^ the wrapped mailbox
wrapWriteHandle = wrapHandle outWrapper

-- | Same as 'wrapReadHandle' but use an existing 'Mailbox' for wrapping.
wrapReadHandleWithMailbox
    :: Wrappable m
    => Handle         -- ^ the handle to wrap
    -> Mailbox m      -- ^ the mailbox to use
    -> ErrorHandler m -- ^ error handler
    -> IO (WrapBox m) -- ^ the wrapped mailbox
wrapReadHandleWithMailbox = wrapHandleWithMailbox inWrapper

-- | Same as 'wrapWriteHandle' but use an existing 'Mailbox' for wrapping.
wrapWriteHandleWithMailbox
    :: Wrappable m
    => Handle         -- ^ the handle to wrap
    -> Mailbox m      -- ^ the mailbox to use
    -> ErrorHandler m -- ^ error handler
    -> IO (WrapBox m) -- ^ the wrapped mailbox
wrapWriteHandleWithMailbox = wrapHandleWithMailbox outWrapper

-- Wrapping (internal) ---------------------------------------------------------

type Wrapper m = (Handle -> Mailbox m -> ErrorHandler m -> IO ())

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
        Right e -> do
            tid <- myThreadId
            errHandler WBox { mBox = mbox, tId = tid } e

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
                Just e  -> do
                    tid <- myThreadId
                    errHandler WBox { mBox = mbox, tId = tid } e
        ]

