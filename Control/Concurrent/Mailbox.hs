module Control.Concurrent.Mailbox
    ( Mailbox
    , MsgHandler

    , (#)

    , newMailbox

    , send
    , (!)

    , receive
    , receiveNonBlocking
    , receiveTimeout
    )
where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Time
import System.Timeout

newtype Mailbox m = MBox (Chan m)

type MsgHandler m = m -> ((), IO ())

(#) :: IO () -> ((), IO ())
(#) = (,) ()

newMailbox :: IO (Mailbox m)
newMailbox = fmap MBox newChan

(!) :: Mailbox m -> m -> IO ()
(!) = send

send :: Mailbox m -> m -> IO ()
send (MBox chan) = writeChan chan

receive :: Mailbox m -> [MsgHandler m] -> IO ()
receive _ [] = return ()
receive (MBox chan) handlers = matchAll chan Nothing handlers

receiveNonBlocking :: Mailbox m -> [MsgHandler m] -> IO ()
receiveNonBlocking (MBox chan) handlers = matchCurrent chan handlers

receiveTimeout :: Mailbox m -> Int -> [MsgHandler m] -> IO ()
receiveTimeout _ _ [] = return ()
receiveTimeout mbox 0 handlers = receiveNonBlocking mbox handlers
receiveTimeout (MBox chan) to handlers = do
    curTime <- getCurrentTime
    let dt = fromIntegral to / 1000000
    matchAll chan (Just $ addUTCTime dt curTime) handlers

matchCurrent :: Chan m -> [MsgHandler m] -> IO ()
matchCurrent chan hs = do
    empty <- isEmptyChan chan
    if empty
        then
            return ()
        else do
            m <- readChan chan
            matched <- match m Nothing hs
            case matched of
                Just False -> do
                    matchCurrent chan hs
                    unGetChan chan m
                Just True ->
                    return ()
                Nothing ->
                    error "Timed out even if no timeout given. This should not happen!"

matchAll :: Chan m -> Maybe UTCTime -> [MsgHandler m] -> IO ()
matchAll chan Nothing hs = do
    m <- readChan chan
    matched <- match m Nothing hs
    case matched of
        Just False -> do
            matchAll chan Nothing hs
            unGetChan chan m
        Just True ->
            return ()
        Nothing ->
            error "Timed out even if no timeout given. This should not happen!"
matchAll chan (Just endTime) hs = do
    curTime <- getCurrentTime
    if curTime >= endTime
        then return ()
        else do
            let to = round $ (diffUTCTime endTime curTime) * 1000000
            mm <- timeout to $ readChan chan

            case mm of
                Just m -> do
                    matched <- match m (Just endTime) hs
                    case matched of
                        Just False -> do
                            matchAll chan (Just endTime) hs
                            unGetChan chan m
                        Just True ->
                            return ()
                        Nothing ->
                            unGetChan chan m
                Nothing ->
                    return ()

match :: m -> Maybe UTCTime -> [MsgHandler m] -> IO (Maybe Bool)
match _ _ [] = return $ Just False
match m Nothing (h : hs) = do
    ma <- catch (case h m of ((), a) -> return $ Just a)
                handlePatternMatchFail
    case ma of
        Just action -> do
            action
            return $ Just True
        Nothing ->
            match m Nothing hs
match m (Just endTime) (h : hs) = do
    curTime <- getCurrentTime
    if curTime >= endTime
        then return Nothing
        else do
            let to = round $ (diffUTCTime endTime curTime) * 1000000
            ma <- timeout to $ catch (case h m of ((), a) -> return $ Just a)
                                     handlePatternMatchFail
            case ma of
                Just (Just action) -> do
                    action
                    return $ Just True
                Just Nothing ->
                    match m (Just endTime) hs
                Nothing -> do
                    return Nothing

handlePatternMatchFail :: PatternMatchFail -> IO (Maybe (IO ()))
handlePatternMatchFail _ = return Nothing

