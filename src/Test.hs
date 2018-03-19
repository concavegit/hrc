module Test where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Network.Socket
import           System.IO

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  chan <- newChan
  _ <- forkIO . fix $ \loop -> do
    (_,_) <- readChan chan
    loop
  mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  _ <- forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "Hi, what's your name?"
  name <- init <$> hGetLine hdl
  broadcast ("--> " ++ name ++ " entered chat.")
  hPutStrLn hdl ("Welcome, " ++ name ++ "!")
  commLine <- dupChan chan
  reader <- forkIO . fix $ \loop -> do
    (nextNum, line) <- readChan commLine
    when (msgNum /= nextNum) $ hPutStrLn hdl line
    loop
  handle (\(SomeException _) -> pure ()) . fix $ \loop -> do
    line <- init <$> hGetLine hdl
    case line of
      "quit" -> hPutStrLn hdl "Bye!"
      _      -> broadcast (name ++ ": " ++ line) >> loop

  killThread reader
  broadcast ("<-- " ++ name ++ " left.")
  hClose hdl
