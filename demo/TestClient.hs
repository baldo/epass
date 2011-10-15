{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import Message
import Control.Concurrent.Mailbox
import Control.Concurrent.Mailbox.Wrapper

import Network
import System.IO

main :: IO ()
main = do
    hdl <- connectTo "" $ UnixSocket "test.socket"

    hPutStr hdl "Test123\n"
    hFlush hdl

    inBox <- wrapReadHandle hdl
                 (\ inBox e -> inBox <! (MsgError $ show e))
    outBox <- wrapWriteHandle hdl
                 (\ _ e -> inBox <! (MsgError $ show e))

    loop inBox outBox 1
    mapM_ close [inBox, outBox]
    hClose hdl

loop :: MailboxClass mb => mb Message -> mb Message -> Int -> IO ()
loop _inBox outBox 1000000 = outBox <! MsgCommand CmdQuit
loop inBox outBox n = do
    outBox <! M n

    receive inBox
        [ \ (MsgError e) -> handler $
                putStrLn $ "received error: " ++ e
        , \ (M 10) -> handler $ do
                putStrLn "received 10"
                loop inBox outBox (n + 1)
        , \ m -> handler $ do
                print m
                loop inBox outBox (n + 1)
        ]

