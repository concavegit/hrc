module Lib where

import           Control.Concurrent
import           Control.Monad.Fix  (fix)
import           Network.Socket
import           System.IO

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  chan <- newChan
  mainLoop sock chan

mainLoop :: Socket -> Chan String -> IO ()
mainLoop sock chan = do
  conn <- accept sock
  _ <- forkIO $ runConn conn chan
  mainLoop sock chan

runConn :: (Socket, SockAddr) -> Chan String -> IO ()
runConn (sock, _) chan = do
  let broadcast = writeChan chan
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  commLine <- dupChan chan

  _ <- forkIO $ fix $ \loop -> do
    line <- readChan commLine
    hPutStrLn hdl line
    loop

  fix $ \loop -> do
    line <- fmap init (hGetLine hdl)
    broadcast line
    loop
