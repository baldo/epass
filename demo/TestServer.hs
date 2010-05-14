module Main where

import Message
import Control.Concurrent.Mailbox
import Control.Concurrent.Mailbox.Wrapper

import Network

main :: IO ()
main = do
    sock <- listenOn $ UnixSocket "test.socket"
    (hdl, _, _) <- accept sock

    (inBox, _)  <- wrapReadHandle hdl
                        (\inBox e -> inBox ! (error $ "Handled: " ++ show e))
    (outBox, _) <- wrapWriteHandle hdl
                        (\_ e -> inBox ! (error $ "Handled: " ++ show e))

    loop inBox outBox

loop :: Mailbox Message -> Mailbox Message -> IO ()
loop inBox outBox = do
    receiveNonBlocking inBox
        [ \(MsgCommand CmdQuit) -> (#) $ return ()
        , \m -> (#) $ do
            putStrLn $ "Matched " ++ show m ++ " non-blocking."
            outBox ! M (-1)
            loop inBox outBox
        ] $ receive inBox
                [ \(M (n + 1)) -> (#) $ do
                    outBox ! M (n * 2)
                    loop inBox outBox
                , \m -> (#) $ do
                    print m
                    loop inBox outBox
                ]
