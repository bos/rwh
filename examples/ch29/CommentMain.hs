module CommentMain (main) where

import Comment (runServer)
import Control.Concurrent.STM

main :: IO ()
main = runServer
