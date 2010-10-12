module Message
    ( Message (..)
    , Command (..)
    )
where

import Control.Concurrent.Mailbox.Wrapper

data Message = MsgError String
             | MsgCommand Command
             | M Int
  deriving (Read, Show)

data Command = CmdQuit
  deriving (Read, Show)

instance Wrappable Message where
    toString = show
    fromStringReadS = reads

