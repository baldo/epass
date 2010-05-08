module Message
    ( Message (..)
    , Event (..)
    , Command (..)
    )
where

import Control.Concurrent.Mailbox.Wrapper

data Message = MsgEvent Event
             | MsgCommand Command
             | M Int
  deriving (Read, Show)

data Event = EvKey Char
  deriving (Read, Show)

data Command = CmdQuit
  deriving (Read, Show)

instance Wrappable Message where
    toString = show
    fromStringReadS = reads

