module Server where

import           Control.Monad
import           Data.ByteString           (ByteString)
import           Network.Socket            hiding (recv, send)
import           Network.Socket.ByteString
import           Pipes
import qualified Pipes.Prelude             as P
import           System.IO

sockInLn :: Socket -> Producer String IO ()
sockInLn sock = do
  eof <- lift isEOF
  unless eof $ do
    str <- lift $ recv sock 10000
    yield (head . lines $ show str)
    sockInLn sock

-- runConn :: (Socket, SockAddr) -> IO ()
-- runConn (sock, _) = do


main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  runEffect $ sockInLn sock >-> P.stdoutLn
