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
                 (\inBox e -> inBox <! (error $ "Handled: " ++ show e))
    outBox <- wrapWriteHandle hdl
                 (\_ e -> inBox <! (error $ "Handled: " ++ show e))

    loop inBox outBox 1

loop :: WrapBox Message -> WrapBox Message -> Int -> IO ()
loop _inBox outBox 1000000 = outBox <! MsgCommand CmdQuit
loop inBox outBox n = do
    outBox <! M n

    receive inBox
        [ \m -> (#) $ do
            print m
            loop inBox outBox (n + 1)
        ]

