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
import Data.Time
import System.Timeout

-- Timeout calculations

timeoutFactor :: Num a => a
timeoutFactor = 1000000

calcEndTime :: Int -> IO UTCTime
calcEndTime to = do
    curTime <- getCurrentTime
    let dt = fromIntegral to / timeoutFactor
    return $ addUTCTime dt curTime

calcTimeLeft :: UTCTime -> IO Int
calcTimeLeft endTime = do
    curTime <- getCurrentTime
    return $ round $ (diffUTCTime endTime curTime) * timeoutFactor

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
receive (MBox chan) handlers = matchAll chan handlers

receiveTimeout :: Mailbox m -> Int -> [MsgHandler m] -> IO ()
receiveTimeout _ _ [] = return ()
receiveTimeout mbox 0 handlers = receiveNonBlocking mbox handlers
receiveTimeout (MBox chan) to handlers = do
    endTime <- calcEndTime to
    matchAllTimeout chan endTime handlers

receiveNonBlocking :: Mailbox m -> [MsgHandler m] -> IO ()
receiveNonBlocking (MBox chan) handlers = matchCurrent chan handlers

matchAll :: Chan m -> [MsgHandler m] -> IO ()
matchAll chan hs = do
    m <- readChan chan
    matched <- match m hs

    if matched
        then
            return ()
        else do
            matchAll chan hs
            unGetChan chan m

matchAllTimeout :: Chan m -> UTCTime -> [MsgHandler m] -> IO ()
matchAllTimeout chan endTime hs = do
    timeLeft <- calcTimeLeft endTime

    if timeLeft <= 0
        then return ()
        else do
            mm <- timeout timeLeft $ readChan chan

            case mm of
                Just m -> do
                    matched <- matchTimeout m endTime hs
                    case matched of
                        Just False -> do
                            matchAllTimeout chan endTime hs
                            unGetChan chan m
                        Just True ->
                            return ()
                        Nothing ->
                            unGetChan chan m
                Nothing ->
                    return ()

matchCurrent :: Chan m -> [MsgHandler m] -> IO ()
matchCurrent chan hs = do
    empty <- isEmptyChan chan
    if empty
        then
            return ()
        else do
            m <- readChan chan
            matched <- match m hs
            if matched
                then
                    return ()
                else do
                    matchAll chan hs
                    unGetChan chan m

match :: m -> [MsgHandler m] -> IO Bool
match _ [] = return False
match m (h : hs) = do
    ma <- catch (case h m of ((), a) -> return $ Just a)
                handlePatternMatchFail
    case ma of
        Just action -> do
            action
            return True
        Nothing ->
            match m hs

matchTimeout :: m -> UTCTime -> [MsgHandler m] -> IO (Maybe Bool)
matchTimeout _ _ [] = return $ Just False
matchTimeout m endTime (h : hs) = do
    timeLeft <- calcTimeLeft endTime
    if timeLeft <= 0
        then return Nothing
        else do
            ma <- timeout timeLeft $
                    catch (case h m of ((), a) -> return $ Just a)
                          handlePatternMatchFail
            case ma of
                Just (Just action) -> do
                    action
                    return $ Just True
                Just Nothing ->
                    matchTimeout m endTime hs
                Nothing -> do
                    return Nothing

handlePatternMatchFail :: PatternMatchFail -> IO (Maybe (IO ()))
handlePatternMatchFail _ = return Nothing

