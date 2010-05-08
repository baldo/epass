import Message
import Control.Concurrent.Mailbox
import Control.Concurrent.Mailbox.Wrapper

import Network
import System.IO
import System.Posix

main :: IO ()
main = do
    hdl <- connectTo "" $ UnixSocket "test.socket"

    hPutStr hdl "Test123\n"
    hFlush hdl

    (outBox, _) <- wrapWriteHandle hdl
    (inBox, _)  <- wrapReadHandle hdl

    loop inBox outBox 1

loop :: Mailbox Message -> Mailbox Message -> Int -> IO ()
loop inBox outBox n = do
    outBox ! M n

    receive inBox
        [ \m -> (#) $ do
            print m
            loop inBox outBox (n + 1)
        ]
