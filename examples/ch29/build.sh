#!/bin/sh

exec ghc -main-is CommentMain.main --make -i../ch18 -o comment -O2 CommentMain
