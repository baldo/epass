{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Concurrent.Mailbox
    ( MailboxClass (..)
    , Mailbox
    , MsgHandler
    , Handler

    , handler

    , newMailbox

    , send
    , (<!)

    , receive
    , receiveNonBlocking
    , receiveTimeout

    , (.>)
    , (<|>)
    )
where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Exception hiding (Handler)
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

class MailboxClass b m where
    getMessage   :: b m -> IO m
    unGetMessage :: b m -> m -> IO ()
    putMessage   :: b m -> m -> IO ()
    isEmpty      :: b m -> IO Bool

newtype Mailbox m = MBox { unMBox :: Chan m }

instance MailboxClass Mailbox m where
    getMessage   = readChan    . unMBox
    unGetMessage = unGetChan   . unMBox
    putMessage   = writeChan   . unMBox
    isEmpty      = isEmptyChan . unMBox

type MsgHandler m a = m -> Handler a

data Handler a = Handler (IO a)

handler :: IO a -> Handler a
handler = Handler

newMailbox :: IO (Mailbox m)
newMailbox = fmap MBox newChan

(<!) :: MailboxClass b m => b m -> m -> IO ()
(<!) = send

send :: MailboxClass b m => b m -> m -> IO ()
send mbox msg = do
    putMessage mbox msg
    yield

receive :: MailboxClass b m => b m -> [MsgHandler m a] -> IO a
receive _ [] = error "No message handler given! Cannot match."
receive mbox handlers = do
    a <- matchAll mbox handlers
    a

receiveTimeout :: MailboxClass b m => b m -> Int -> [MsgHandler m a] -> IO a -> IO a
receiveTimeout _ _ [] toa = toa
receiveTimeout mbox 0 handlers toa = receiveNonBlocking mbox handlers toa
receiveTimeout mbox to handlers toa = do
    endTime <- calcEndTime to
    ma <- matchAllTimeout mbox endTime handlers
    case ma of
        Just a  -> a
        Nothing -> toa

receiveNonBlocking :: MailboxClass b m => b m -> [MsgHandler m a] -> IO a -> IO a
receiveNonBlocking mbox handlers na = do
    ma <- matchCurrent mbox handlers
    case ma of
        Just a  -> a
        Nothing -> na

matchAll :: MailboxClass b m => b m -> [MsgHandler m a] -> IO (IO a)
matchAll mbox hs = do
    m <- getMessage mbox
    ma <- match m hs

    case ma of
        Just a ->
            return a

        Nothing -> do
            r <- matchAll mbox hs
            unGetMessage mbox m
            return r

matchAllTimeout :: MailboxClass b m => b m -> UTCTime -> [MsgHandler m a] -> IO (Maybe (IO a))
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
                        Left Nothing -> do
                            r <- matchAllTimeout mbox endTime hs
                            unGetMessage mbox m
                            return r

                        Left (Just a) ->
                            return $ Just a

                        Right () -> do
                            unGetMessage mbox m
                            return Nothing
                Nothing ->
                    return Nothing

matchCurrent :: MailboxClass b m => b m -> [MsgHandler m a] -> IO (Maybe (IO a))
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

match :: m -> [MsgHandler m a] -> IO (Maybe (IO a))
match _ [] = return Nothing
match m (h : hs) = do
    ma <- catch (case h m of (Handler a) -> return $ Just a)
                handlePatternMatchFail
    case ma of
        Just action -> return $ Just action
        Nothing     -> match m hs

matchTimeout :: m -> UTCTime -> [MsgHandler m a] -> IO (Either (Maybe (IO a)) ())
matchTimeout _ _ [] = return $ Left Nothing
matchTimeout m endTime (h : hs) = do
    timeLeft <- calcTimeLeft endTime
    if timeLeft <= 0
        then return $ Right ()
        else do
            ma <- timeout timeLeft $
                    catch (case h m of (Handler a) -> return $ Just a)
                          handlePatternMatchFail
            case ma of
                Just (Just action) -> return $ Left $ Just action
                Just Nothing       -> matchTimeout m endTime hs
                Nothing            -> return $ Right ()

handlePatternMatchFail :: PatternMatchFail -> IO (Maybe (IO a))
handlePatternMatchFail _ = return Nothing

(.>) :: MsgHandler m a -> (a -> b) -> MsgHandler m b
(h .> f) m = 
    let Handler a = h m 
    in  handler $ a >>= return . f

(<|>) :: [MsgHandler m a] -> [MsgHandler m b] -> [MsgHandler m (Either a b)]
has <|> hbs = (map (.> Left) has) ++ (map (.> Right) hbs)

