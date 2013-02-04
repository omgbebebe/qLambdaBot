module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy (send, recv)
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment (getArgs)
 
import qualified QBotNetwork as QB
import qualified Text.JSON as J

main :: IO ()
main = do
  [host', port'] <- getArgs
  client host' (read port' :: Int)
 
client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
  clientDone <- newEmptyMVar
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  forkIO $ msgSender sock clientDone
  takeMVar clientDone
  sClose sock
 
msgSender :: Socket -> MVar Bool -> IO ()
msgSender sock clientDone = do
--  msg <- B8.getLine
--  let msg = B.pack (0 :: Word8)
  let magic         = encode (0xDEAD :: Word16)
      message'      = J.encode QB.GetAllUnits
      message       = B8.pack message'
      len           = (fromIntegral $ length message') :: Word32
      messageLength = encode len
--      messageLength = encode (0xFFFFFFFF :: Word32)
--      msg = B.concat([magic, messageLength, message])
      msg = B.concat([magic, messageLength, message])
  send sock msg
--  putMVar clientDone True
--  threadDelay 100000
--  magic <- recv sock 2
--  l <- recv sock 4
  rMsg <- recv sock 1000
  B8.putStrLn rMsg
  msgSender sock clientDone
--  if msg == encode (1 :: Word8) then putMVar clientDone True else msgSender sock clientDone