module SimpleServer where

import Data.ByteString 
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import System.IO
import Control.Exception
import Control.Concurrent

data Server = Server Int

runServer :: String -> (ByteString -> IO a) -> IO ()
runServer port f = do
  let hints = defaultHints {
        addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 2    
  runServerLoop sock f

runServerLoop :: Socket -> (ByteString -> IO a) -> IO ()
runServerLoop sock f = do
  conn <- accept sock
  forkIO $ runServerConn conn f
  runServerLoop sock f

runServerConn :: (Socket, SockAddr) -> (ByteString -> IO a) -> IO ()
runServerConn (sock, addr) f = do
  line <- recv sock 64
  f line
  runServerConn (sock, addr) f

{--
-- A simple echo server running on port 8080
main :: IO ()
main = runServer 8080 (\s -> putStrLn s)
--}
