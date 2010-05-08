import Message
import Control.Concurrent.Mailbox
import Control.Concurrent.Mailbox.Wrapper
import System.Posix

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
    usleep 1000
    receiveNonBlocking inBox
        [ \m -> (#) $ do
            putStrLn $ "Matched " ++ show m ++ " non-blocking."
            outBox ! M (-1)
            loop inBox outBox
        ] $
        receiveTimeout inBox 1000
            [ \m -> (#) $ do
                putStrLn $ "Matched " ++ show m ++ " within timeout."
                outBox ! M (-1)
                loop inBox outBox
            ] $
            receive inBox
                [ \(M (n + 1)) -> (#) $ do
                    outBox ! M (n * 2)
                    loop inBox outBox
                , \m -> (#) $ do
                    print m
                    loop inBox outBox
                ]
