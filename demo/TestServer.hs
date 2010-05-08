import Message
import Control.Concurrent.Mailbox
import Control.Concurrent.Mailbox.Wrapper

import Network

main :: IO ()
main = do
    sock <- listenOn $ UnixSocket "test.socket"
    (hdl, _, _) <- accept sock

    (inBox, _)  <- wrapReadHandle hdl
    (outBox, _) <- wrapWriteHandle hdl

    loop inBox outBox

loop :: Mailbox Message -> Mailbox Message -> IO ()
loop inBox outBox = do
    receiveTimeout inBox 100000
        [ \(M 5) -> (#) $ do
            putStrLn "Matched 5 within timeout."
            outBox ! M (-1)
        ]
    receive inBox
        [ \(M (n + 1)) -> (#) $ outBox ! M (n * 2)
        , \m           -> (#) $ print m
        ]

    loop inBox outBox
