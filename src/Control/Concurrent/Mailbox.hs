{- | This module provides Erlang like functionality for message passing.

     Instead of mailboxes attached to each process you have to create the needed
     mailboxes yourself. This means that messages cannot be send to processes
     or threads directly, but only to mailboxes. On the other hand multiple
     threads may share a mailbox and one thread may have multiple mailboxes.

     For a simple example on how to receive messages have a look at the
     'MsgHandler' type.
-}

module Control.Concurrent.Mailbox
    ( 
    -- * Mailbox
      MailboxClass (..)
    , Mailbox

    , newMailbox

    -- * Sending messages
    , send
    , (<!)

    -- * Receiving messages
    , receive
    , receiveNonBlocking
    , receiveTimeout

    -- * Message handlers
    , MsgHandler
    , Handler

    , handler

    -- * Message handler combinators
    , (.>)
    , (<|>)
    )
where

import Prelude hiding (catch)

import Control.Concurrent (yield)
import Control.Concurrent.STM
import Control.Exception hiding (Handler)
import Data.Time
import System.Timeout


-- Mailbox ---------------------------------------------------------------------

{- | Any instance of 'MailboxClass' may be used as a mailbox for message
     passing. @b@ is the mailbox type and m is the message type.
-}
class MailboxClass b where
    -- | Get a message from the mailbox (with 'Mailbox' it is the first one).
    getMessage
        :: b m  -- ^ the mailbox
        -> IO m -- ^ the message

    {- | Put a message back to the mailbox (with 'Mailbox' it will be placed
         at the beginning of the mailbox).
    -}
    unGetMessage
        :: b m -- ^ the mailbox
        -> m   -- ^ the message
        -> IO ()

    {- | Add a new message to the mailbox (with 'Mailbox' it will be placed at
         the end of the mailbox).
    -}
    putMessage
        :: b m -- ^ the mailbox
        -> m   -- ^ the message
        -> IO ()

    -- | Checks wether the mailbox is empty.
    isEmpty
        :: b m     -- ^ the mailbox
        -> IO Bool -- ^ 'True' if empty

    {- | Call this function to cleanup before exit or when the mailbox is no
         longer needed.
    -}
    close
        :: b m
        -> IO ()

-- | A 'Chan' based mailbox.
newtype Mailbox m = MBox (TChan m)

-- | Creates a new mailbox.
newMailbox :: IO (Mailbox m)
newMailbox = fmap MBox newTChanIO

instance MailboxClass Mailbox where
    getMessage   (MBox chan)     = atomically $ readTChan chan
    unGetMessage (MBox chan) msg = atomically $ unGetTChan chan msg
    putMessage   (MBox chan) msg = atomically $ writeTChan chan msg
    isEmpty      (MBox chan)     = atomically $ isEmptyTChan chan
    close        _               = return ()


-- Sending messages ------------------------------------------------------------

-- | Send the given message to the given mailbox.
send
    :: MailboxClass b
    => b m -- ^ the mailbox
    -> m   -- ^ the message
    -> IO ()
send mbox msg = do
    putMessage mbox msg
    yield

-- | An alias for 'send' in the flavor of Erlang's @!@.
(<!)
    :: MailboxClass b
    => b m
    -> m
    -> IO ()
(<!) = send


-- Timeout calculations (internal) ---------------------------------------------

timeoutFactor
    :: Num a
    => a
timeoutFactor = 1000000

calcEndTime
    :: Int
    -> IO UTCTime
calcEndTime to = do
    curTime <- getCurrentTime
    let dt = fromIntegral to / timeoutFactor
    return $ addUTCTime dt curTime

calcTimeLeft
    :: UTCTime
    -> IO Int
calcTimeLeft endTime = do
    curTime <- getCurrentTime
    return $ round $ (diffUTCTime endTime curTime) * timeoutFactor


-- Receiving messages ----------------------------------------------------------

{- | Receive messages in the flavour of Erlang's @receive@.

     For each message in the mailbox all message handlers are matched until a
     matching message is found. It will be removed from the mailbox and the
     matching message handler's action will be performed.

     If no message matches any of the message handler, 'receive' will block and
     check new incoming messages until a match is found.
-}
receive
    :: MailboxClass b
    => b m              -- ^ mailbox to receive on
    -> [MsgHandler m a] -- ^ message handlers
    -> IO a
receive _    []       = error "No message handler given! Cannot match."
receive mbox handlers = do
    a <- matchAll mbox handlers
    a

{- | Like 'receive', but times out after a given time. In case of timeout the
     timeout handler is executed.
-}
receiveTimeout
    :: MailboxClass b
    => b m              -- ^ the mailbox
    -> Int              -- ^ timeout in us
    -> [MsgHandler m a] -- ^ message handlers
    -> IO a             -- ^ timeout handler
    -> IO a
receiveTimeout _    _ []        toa = toa
receiveTimeout mbox 0  handlers toa = receiveNonBlocking mbox handlers toa
receiveTimeout mbox to handlers toa = do
    endTime <- calcEndTime to
    ma <- matchAllTimeout mbox endTime handlers
    case ma of
        Just a  -> a
        Nothing -> toa

{- | Like 'receive', but doesn't block. If no match was found, the default
     handler is executed.
-}
receiveNonBlocking
    :: MailboxClass b
    => b m              -- ^ the mailbox
    -> [MsgHandler m a] -- ^ message handlers
    -> IO a             -- ^ default handler
    -> IO a
receiveNonBlocking mbox handlers na = do
    ma <- matchCurrent mbox handlers
    case ma of
        Just a  -> a
        Nothing -> na


-- Matching messages (internal) ------------------------------------------------

data TimeoutResult a = Match (IO a)
                     | NoMatch
                     | Timeout

matchAll
    :: MailboxClass b
    => b m
    -> [MsgHandler m a]
    -> IO (IO a)
matchAll mbox hs = do
    m  <- getMessage mbox
    ma <- match m hs

    case ma of
        Just a ->
            return a

        Nothing -> do
            r <- matchAll mbox hs
            unGetMessage mbox m
            return r

matchAllTimeout
    :: MailboxClass b
    => b m
    -> UTCTime
    -> [MsgHandler m a]
    -> IO (Maybe (IO a))
matchAllTimeout mbox endTime hs = do
    timeLeft <- calcTimeLeft endTime

    if timeLeft <= 0
        then return Nothing
        else do
            mm <- timeout timeLeft $ getMessage mbox

            case mm of
                Just m -> do
                    matched <- matchTimeout m endTime hs
                    case matched of
                        NoMatch -> do
                            r <- matchAllTimeout mbox endTime hs
                            unGetMessage mbox m
                            return r

                        (Match a) ->
                            return $ Just a

                        Timeout -> do
                            unGetMessage mbox m
                            return Nothing
                Nothing ->
                    return Nothing

matchCurrent
    :: MailboxClass b
    => b m
    -> [MsgHandler m a]
    -> IO (Maybe (IO a))
matchCurrent mbox hs = do
    empty <- isEmpty mbox 
    if empty
        then return Nothing
        else do
            m  <- getMessage mbox
            ma <- match m hs
            case ma of
                Just a ->
                    return $ Just a

                Nothing -> do
                    r <- matchCurrent mbox hs
                    unGetMessage mbox m
                    return r

match
    :: m
    -> [MsgHandler m a]
    -> IO (Maybe (IO a))
match _ []       = return Nothing
match m (h : hs) = do
    ma <- catch (case h m of (Handler a) -> return $ Just a)
                handlePatternMatchFail
    case ma of
        Just action -> return $ Just action
        Nothing     -> match m hs

matchTimeout
    :: m
    -> UTCTime
    -> [MsgHandler m a]
    -> IO (TimeoutResult a)
matchTimeout _ _       []       = return NoMatch
matchTimeout m endTime (h : hs) = do
    timeLeft <- calcTimeLeft endTime
    if timeLeft <= 0
        then return Timeout
        else do
            ma <- timeout timeLeft $
                    catch (case h m of (Handler a) -> return $ Just a)
                          handlePatternMatchFail
            case ma of
                Just (Just action) -> return $ Match action
                Just Nothing       -> matchTimeout m endTime hs
                Nothing            -> return $ Timeout

handlePatternMatchFail
    :: PatternMatchFail
    -> IO (Maybe (IO a))
handlePatternMatchFail _ = return Nothing


-- Message handlers ------------------------------------------------------------

{- | A function that matches a given message and returns the corresponding
     handler.

     In case of an pattern matching error 'receive' will continue matching
     the next 'MsgHandler' / message.

     For example you may write somthing like this:

     > receive mbox
     >     [ \ True  -> handler $ return 1
     >     , \ False -> handler $ return 2
     >     ]
-}
type MsgHandler m a = m -> Handler a

-- | The action to perfom in case of successful matching.
data Handler a = Handler (IO a)

-- | Generate a handler from an 'IO' action.
handler
    :: IO a
    -> Handler a
handler = Handler


-- Message handler combinators -------------------------------------------------

-- | Apply a function to the result of an message handler.
(.>)
    :: MsgHandler m a -- ^ message handler
    -> (a -> b)       -- ^ function
    -> MsgHandler m b -- ^ new message handler
(h .> f) m = 
    let Handler a = h m 
    in  handler $ a >>= return . f

{- | Combine to lists of message handlers into one list. The results of the
     message handler will be wrapped in 'Either'.
-}
(<|>)
    :: [MsgHandler m a]            -- ^ message handlers
    -> [MsgHandler m b]            -- ^ more message handlers
    -> [MsgHandler m (Either a b)] -- ^ combined message handlers
has <|> hbs = (map (.> Left) has) ++ (map (.> Right) hbs)

