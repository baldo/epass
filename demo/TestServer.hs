module Main where

import Message
import Control.Concurrent.Mailbox
import Control.Concurrent.Mailbox.Wrapper

import Network
import System.IO

main :: IO ()
main = do
    sock <- listenOn $ UnixSocket "test.socket"
    (hdl, _, _) <- accept sock

    inBox <- wrapReadHandle hdl
                 (\inBox e -> inBox <! (error $ "Handled: " ++ show e))
    outBox <- wrapWriteHandle hdl
                 (\_ e -> inBox <! (error $ "Handled: " ++ show e))

    loop inBox outBox
    mapM close [inBox, outBox]
    hClose hdl

loop :: MailboxClass mb => mb Message -> mb Message -> IO ()
loop inBox outBox = do
    receiveNonBlocking inBox
        [ \ (MsgCommand CmdQuit) -> handler $ return ()
        , \ m -> handler $ do
            putStrLn $ "Matched " ++ show m ++ " non-blocking."
            outBox <! M (-1)
            loop inBox outBox
        ] $ receive inBox
                [ \ (M (n + 1)) -> handler $ do
                    outBox <! M (n * 2)
                    loop inBox outBox
                , \ m -> handler $ do
                    print m
                    loop inBox outBox
                ]
