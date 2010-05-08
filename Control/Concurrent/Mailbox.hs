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
receive (MBox chan) handlers = do
    a <- matchAll chan handlers
    a

receiveTimeout :: Mailbox m -> Int -> [MsgHandler m] -> IO () -> IO ()
receiveTimeout _ _ [] toa = toa
receiveTimeout mbox 0 handlers toa = receiveNonBlocking mbox handlers toa
receiveTimeout (MBox chan) to handlers toa = do
    endTime <- calcEndTime to
    ma <- matchAllTimeout chan endTime handlers
    case ma of
        Just a  -> a
        Nothing -> toa

receiveNonBlocking :: Mailbox m -> [MsgHandler m] -> IO () -> IO ()
receiveNonBlocking (MBox chan) handlers na = do
    ma <- matchCurrent chan handlers
    case ma of
        Just a  -> a
        Nothing -> na

matchAll :: Chan m -> [MsgHandler m] -> IO (IO ())
matchAll chan hs = do
    m <- readChan chan
    ma <- match m hs

    case ma of
        Just a ->
            return a

        Nothing -> do
            r <- matchAll chan hs
            unGetChan chan m
            return r

matchAllTimeout :: Chan m -> UTCTime -> [MsgHandler m] -> IO (Maybe (IO ()))
matchAllTimeout chan endTime hs = do
    timeLeft <- calcTimeLeft endTime

    if timeLeft <= 0
        then return Nothing
        else do
            mm <- timeout timeLeft $ readChan chan

            case mm of
                Just m -> do
                    matched <- matchTimeout m endTime hs
                    case matched of
                        Left Nothing -> do
                            r <- matchAllTimeout chan endTime hs
                            unGetChan chan m
                            return r

                        Left (Just a) ->
                            return $ Just a

                        Right () -> do
                            unGetChan chan m
                            return Nothing
                Nothing ->
                    return Nothing

matchCurrent :: Chan m -> [MsgHandler m] -> IO (Maybe (IO ()))
matchCurrent chan hs = do
    empty <- isEmptyChan chan
    if empty
        then return Nothing
        else do
            m  <- readChan chan
            ma <- match m hs
            case ma of
                Just a ->
                    return $ Just a

                Nothing -> do
                    r <- matchCurrent chan hs
                    unGetChan chan m
                    return r

match :: m -> [MsgHandler m] -> IO (Maybe (IO ()))
match _ [] = return Nothing
match m (h : hs) = do
    ma <- catch (case h m of ((), a) -> return $ Just a)
                handlePatternMatchFail
    case ma of
        Just action -> return $ Just action
        Nothing     -> match m hs

matchTimeout :: m -> UTCTime -> [MsgHandler m] -> IO (Either (Maybe (IO ())) ())
matchTimeout _ _ [] = return $ Left Nothing
matchTimeout m endTime (h : hs) = do
    timeLeft <- calcTimeLeft endTime
    if timeLeft <= 0
        then return $ Right ()
        else do
            ma <- timeout timeLeft $
                    catch (case h m of ((), a) -> return $ Just a)
                          handlePatternMatchFail
            case ma of
                Just (Just action) -> return $ Left $ Just action
                Just Nothing       -> matchTimeout m endTime hs
                Nothing            -> return $ Right ()

handlePatternMatchFail :: PatternMatchFail -> IO (Maybe (IO ()))
handlePatternMatchFail _ = return Nothing

