module HRC where

import           Network.Socket
import           Pipes
import           System.IO

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  runEffect $ hello sock

hello :: Socket -> Effect IO ()
hello sock = do
  conn <- lift $ accept sock
  lift $ runConn conn
  hello sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "Hello!"
  hClose hdl
