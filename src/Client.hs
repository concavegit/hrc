{-# LANGUAGE RecordWildCards #-}

module Client where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Network
import           System.IO
import           Text.Printf

type ClientName = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String


newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  pure Client
    { clientName = name
    , clientHandle = handle
    , clientKicked = k
    , clientSendChan = c
    }

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan

newtype Server = Server
  { clients :: TVar (Map ClientName Client)
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  pure Server { clients = c }

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (`sendMessage` msg) (Map.elems clientmap)

main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle server) (\_ -> hClose handle)

port :: Int
port = 44444

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then pure Nothing
    else do
    client <- newClient name handle
    writeTVar clients $ Map.insert name client clientmap
    broadcast server $ Notice (name ++ "has connected")
    pure (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
  where
    readName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      if null name
        then readName
        else mask $ \restore -> do
        ok <- checkAddClient server name handle
        case ok of
          Nothing -> restore $ do
            hPrintf handle "try again; %s in use" name
            readName
          Just client -> restore (runClient server client) `finally` removeClient server name

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  _ <- race server receive
  pure ()
  where receive = forever $ do
          msg <- hGetLine clientHandle
          atomically $ sendMessage client (Command msg)
        server = join $ atomically $ do
          k <- readTVar clientKicked
          case k of
            Just reason -> return $
              hPutStrLn clientHandle $ "You have been kicked: " ++ reason
            Nothing -> do
              msg <- readTChan clientSendChan
              pure $ do
                continue <- handleMessage serv client msg
                when continue server

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage _ Client{..} (Notice msg) = output $ "*** " ++ msg
  where output s = hPutStrLn clientHandle s >> pure True
handleMessage _ Client{..} (Tell name msg) = output $ "*" ++ name ++ "*: " ++ msg
  where output s = hPutStrLn clientHandle s >> pure True
handleMessage _ Client{..} (Broadcast name msg) = output $ "<" ++ name ++ ">: " ++ msg
  where output s = hPutStrLn clientHandle s >> pure True
handleMessage server client@Client{..} (Command msg) =
  case words msg of
    ["/kick", who] -> do
      atomically $ kick server who clientName
      pure True
    "/tell":who:what -> do
      tell server client who (unwords what)
      pure True
    ["/quit"] -> pure False
    ('/':_):_ -> do
      hPutStrLn clientHandle $ "Unrecognized command: " ++ msg
      pure True
    _ -> do
      atomically $ broadcast server $ Broadcast clientName msg
      return True

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  clientmap <- readTVar clients
  case Map.lookup who clientmap of
    Nothing -> void $ sendToName server by (Notice $ who ++ " is not connected")
    Just victim -> do
      writeTVar (clientKicked victim) $ Just ("by " ++ by)
      void $ sendToName server by (Notice $ "you kicked" ++ who)

sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case Map.lookup name clientmap of
    Nothing     -> return False
    Just client -> sendMessage client msg >> pure True

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  if ok
    then pure ()
    else hPutStrLn clientHandle (who ++ " is not connected.")
