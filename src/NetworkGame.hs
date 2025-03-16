{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NetworkGame where

import Game
import Buffer

import SDL (delay, ticks, initializeAll, Event, createWindow, createRenderer, destroyWindow, destroyRenderer)
import qualified SDL.Font

import Data.Aeson
import qualified Data.ByteString as S 
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock.POSIX
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Control.Exception as E
import Control.Monad (when, unless, forever, void, (=<<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State as St
import Control.Monad.Trans.Reader
import Control.DeepSeq

import Network.Socket
import Network.Socket.ByteString  (recv, sendAll)

import System.IO

import Debug.Trace

-- Client -(talk)> Chan -(updateGamestate)> MVar GameState -- Is it necessary maybe if we get really laggy updateGamestates och games becouse they are on a timer, and if we want to add udp. That brings evan more problems but we will need que / buffer.

type UnixTime = Int -- Milli seconds


getUnixTime :: IO UnixTime
getUnixTime =
  do t <- getPOSIXTime
     return $ round (t * 1000)

data NetworkConfig = MkNetworkConfig {hostName :: Maybe HostName -- can this be used for clients hmm maybe
                                     ,sendDelay :: Int -- right now only for client but it could matter if i change to udp
                                     ,gameconf :: GameConfig}

defaultNetConf = MkNetworkConfig Nothing 1000 defaultConfig

recive :: Socket -> IO S.ByteString -- stopped by null or @ at end of message
recive = accum mempty
  where
    accum :: S.ByteString -> Socket -> IO S.ByteString
    accum message s = do 
      msg <- recv s 1024      
      case (S.null msg) of
        False -> do
          case C.last msg of
            '@' -> do
              return (message <> (S.init msg))
            _   -> do
              accum (message <> msg) s
        True  -> do
          return (message <> msg)   


sendSafe :: Socket -> S.ByteString -> IO ()
sendSafe s = sendAll s . (flip C.snoc '@') -- pointfree yay?

type MilliSeconds = Int
type PlayerID     = Int

runGameServerTCPPlayerID :: forall gamestate clientMessage serverMessage.
  (NFData gamestate, FromJSON clientMessage, ToJSON serverMessage)
                 => NetworkConfig
                 -> (gamestate -> Float -> gamestate)
                 -> (PlayerID -> MilliSeconds -> clientMessage -> gamestate -> gamestate)
                 -> (PlayerID -> gamestate -> serverMessage)
                 -> gamestate -> IO ()
runGameServerTCPPlayerID conf update updateOnRecv messageSend initialGameState =
  do hSetBuffering stdout NoBuffering
     initializeAll
     reciveBuffer <- newEmptyBuffer
     gameMVar     <- newMVar initialGameState
     sendMVar     <- (newMVar M.empty :: IO (MVar (M.Map PlayerID (MVar serverMessage))))
     connected    <- ((newMVar []) :: IO (MVar [PlayerID]))
     forkIO $ runTCPServer (hostName conf) "3000" (talk connected reciveBuffer sendMVar)
     forkIO $ handleClientMessage gameMVar reciveBuffer
     simulateGame gameMVar sendMVar connected
  where gameUpdate gameMVar sendMVar connected f =
          do g <- takeMVar gameMVar
             connects <- readMVar connected
             let g' = update g f

             sendMap <- readMVar sendMVar
             mapM tryTakeMVar (M.elems sendMap)
             g' `deepseq` (mapM (\(key, mvar) -> putMVar mvar $ messageSend key g') $ M.assocs sendMap)
             
             putMVar gameMVar g' 
             
        simulateGame gameMVar sendMVar connected = ticks >>= (\initialTicks -> 
         runStateT (loopTimed (fps (gameconf conf)) (\f ->
                           (do t <- St.get
                               t' <- liftIO ticks
                               let delta = traceShowId (fromIntegral (t' - t) / 1000) -- conversion to seconds
                               liftIO $ gameUpdate gameMVar sendMVar connected delta

                               St.put t'
                               return ()))) initialTicks) >> return ()

                                                     
             
        handleClientMessage gameMVar reciveBuffer = do          
          (pID, (t, message)) <- readBuffer reciveBuffer
          t' <- getUnixTime
          modifyMVar_ gameMVar (return . updateOnRecv pID  (t' - t) message)
          handleClientMessage gameMVar reciveBuffer


runGameServerTCP :: forall gamestate clientMessage serverMessage. (NFData gamestate, FromJSON clientMessage, ToJSON serverMessage) => NetworkConfig
                                                                   -> (gamestate -> Float -> gamestate)
                                                                   -> (MilliSeconds -> clientMessage -> gamestate -> gamestate)
                                                                   -> (gamestate -> serverMessage)
                                                                   -> gamestate -> IO ()
runGameServerTCP conf update updateOnRecv messageSend initialGameState = runGameServerTCPPlayerID conf update (const updateOnRecv) (const messageSend) initialGameState

--runGameServerTCPStartup :: 


runGameClientTCP :: (ToJSON clientMessage, FromJSON serverMessage) => forall gamestate. NetworkConfig
                                                                   -> (gamestate -> Float -> gamestate)
                                                                   -> (MilliSeconds -> serverMessage -> gamestate -> gamestate)
                                                                   -> (gamestate -> clientMessage)
                                                                   -> RenderingContext gamestate
                                                                   -> (gamestate -> Int -> GraphicalProcess)
                                                                   -> (Event -> gamestate -> gamestate)
                                                                   ->  IO ()
runGameClientTCP conf update updateOnRecv messageSend getInitialGame drawGame eventHandler =
  do hSetBuffering stdout NoBuffering
     initializeAll
     reciveBuffer <- newEmptyBuffer
     gameMVar     <- newEmptyMVar
     sendMVar     <- newEmptyMVar :: IO (MVar clientMessage)
     forkIO $ runTCPClient (fromJust $ hostName conf) "3000" (clientTalk sendMVar reciveBuffer)
     forkIO $ handleServerMessage gameMVar reciveBuffer
     runMVarGame gameMVar sendMVar
     
     --     runGameIO (gameconf conf) (liftIO (putMVar gameMVar) =<< getInitialGame) drawGame (gameUpdate gameMVar) eventHandler
 where handleServerMessage gameMVar reciveBuffer = forever $
        (do          
          (t, message) <- readBuffer reciveBuffer
          t' <- getUnixTime
--          print (t' - t)
          modifyMVar_ gameMVar (return . updateOnRecv (t' - t) message))

---       runMVarGame :: MVar gamestate -> IO ()
       runMVarGame gameMVar sendMVar =
         do SDL.Font.initialize
            let config = gameconf conf 
            win <- createWindow (T.pack "test") $ windowConf config
            render <- createRenderer win (-1) $ renderConf config

            runReaderT getInitialGame render >>= putMVar gameMVar

            runMVarLoop gameMVar sendMVar render 
            
            destroyRenderer render -- dunno if i have to do it but its sdl so who knows
            destroyWindow win

       runMVarLoop gameMVar sendMVar render = ticks >>= (\initialTicks -> 
         runStateT (loopTimedExit (fps (gameconf conf)) (\f ->
                           (do (framecount, t) <- St.get
                               t' <- liftIO ticks
                               let delta = traceShowId (fromIntegral (t' - t) / 1000) -- conversion to seconds
                               g <- liftIO $ takeMVar gameMVar
                               (g', exits, _) <- liftIO $ gameStep (gameconf conf) render drawGame (\f -> \g -> return $ update f g) eventHandler g framecount delta

                               liftIO $ tryTakeMVar sendMVar
                               liftIO $ putMVar sendMVar (messageSend g')
                               
                               liftIO $ putMVar gameMVar g'
                               St.put (framecount + 1, t')
                               return exits))) (0, initialTicks)) >> return ()


       
talk :: (FromJSON clientMessage, ToJSON serverMessage) => MVar [PlayerID] -> Buffer (PlayerID, (UnixTime, clientMessage)) -> MVar (M.Map PlayerID (MVar serverMessage)) -> Int -> Socket -> IO ()
talk connected reciveBuffer sendMVar ind s =  
  do modifyMVar connected (\connects -> return (if ind `elem` connects then connects else ind : connects, ()))
     mesMap <- readMVar sendMVar
     when (ind `M.notMember` mesMap) (newEmptyMVar
                                        >>= (\mvar -> swapMVar sendMVar (M.insert ind mvar mesMap))
                                        >> return ())
     message <- fmap decodeStrict $ ((\x -> return (C.unpack x) >> return x) =<< recive s)
     writeBuffer reciveBuffer (ind, fromJust message)
     time <- getUnixTime
     messageMap <- readMVar sendMVar
     sending <- readMVar (messageMap M.! ind)
     sendSafe s $ C.toStrict $ Data.Aeson.encode (time, sending)
     talk connected reciveBuffer sendMVar ind s


clientTalk :: (ToJSON clientMessage, FromJSON serverMessage) => MVar clientMessage -> Buffer (UnixTime, serverMessage) -> Socket -> IO ()
clientTalk sendMVar reciveBuffer s =
  do time <- getUnixTime
     putStrLn "Sending Packages"
     sendSafe s =<< (fmap (\x -> C.toStrict $ Data.Aeson.encode (time, x)) $ readMVar sendMVar)
     message <- fmap decodeStrict $ recive s
     writeBuffer reciveBuffer (fromJust message)
     delay 100
     clientTalk sendMVar reciveBuffer s
     

runTCPServer :: Maybe HostName -> ServiceName -> (Int -> Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close (sloop 0)
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1

        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr 
        listen sock 1024
        return sock
    sloop n sock = do
      E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server n conn) (const $ gracefulClose conn 5000)
         >> putStrLn ("connecting nm: " ++ show n)
      sloop (n+1) sock


runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
