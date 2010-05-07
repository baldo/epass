module Control.Concurrent.Mailbox
    ( Mailbox
    , MsgHandler

    , newMailbox
    , send
    , (!)
    , receive
    )
where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Exception
import Control.Monad

newtype Mailbox m = MBox (Chan m)

type MsgHandler m = m -> IO ()

newMailbox :: IO (Mailbox m)
newMailbox = fmap MBox newChan

(!) :: Mailbox m -> m -> IO ()
(!) = send

send :: Mailbox m -> m -> IO ()
send (MBox chan) = writeChan chan

receive :: Mailbox m -> [MsgHandler m] -> IO ()
receive _ [] = return ()
receive (MBox chan) handlers = matchAll chan handlers
    
matchAll :: Chan m -> [MsgHandler m] -> IO ()
matchAll chan hs = do
    m <- readChan chan
    matched <- match m hs
    unless matched $ do
        matchAll chan hs
        unGetChan chan m

match :: m -> [MsgHandler m] -> IO Bool
match _ [] = return False
match m (h : hs) = do
    matched <- catch (h m >> return True) handlePatternMatchFail
    if matched
        then
            return True
        else
            match m hs

handlePatternMatchFail :: PatternMatchFail -> IO Bool
handlePatternMatchFail _ = return False

