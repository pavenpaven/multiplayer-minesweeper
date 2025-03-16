{-# LANGUAGE DeriveGeneric #-}

module Main where

import FrontEnd
import Mines
import NetworkGame
import Game
import Buffer

import Data.Aeson
import Data.Aeson.Decoding
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Map.Strict (insert, insertWith, (!), (!?)) 
import qualified Data.ByteString.Lazy as B

import Control.Monad (join)
import Control.DeepSeq
import GHC.Generics
import System.Random

import Debug.Trace

data ServerState = MkServerState {actions :: M.Map PlayerID (Maybe [Action]) -- Nothing if not initialzed
                                 ,conformationActions :: M.Map PlayerID [Action]
                                 ,seed    :: Seed}
  deriving (Generic, Show)

instance NFData Action
instance NFData ServerState

listSizeLimit :: Int
listSizeLimit = 30

clientUpdate :: PlayerID -> MilliSeconds -> ClientMessage -> ServerState -> ServerState
clientUpdate pID _ mes state = let s = if pID `M.notMember` playerMap
                                          then state {actions = insert pID Nothing playerMap, conformationActions = insert pID [] (conformationActions state)}  -- TODO Fix this 
                                          else state
                               in case mes of
                                      Uninitialized              -> s {actions = insert pID Nothing playerMap}
                                      Initialized actionsMessage -> let state' = s {actions = fmap (fmap ((take listSizeLimit . (actionsMessage ++ )))) playerMap
                                                                                   ,conformationActions = insert pID actionsMessage (conformationActions s)}
                                                                    in state' {actions = M.update (\x -> Just (Just $ fromMaybe [] (actions state' ! pID))) pID $ actions state'}      
      where playerMap = actions state

clientSend :: PlayerID -> ServerState -> ServerMessage
clientSend pID s = fromMaybe (Init (seed s)) ((\x -> return $ Upd (x, conformationActions s ! pID)) =<< join (actions s !? pID))

main :: IO ()
main =
  do decoded <- fmap eitherDecode $ B.readFile confFile
     case decoded of
       Left s         -> putStrLn s
       Right userconf ->
         (do let host = host_name userconf
             s <- fmap (fst . next) getStdGen
             runGameServerTCPPlayerID
               (defaultNetConf
                {hostName = Just host})
               (\s f -> traceShowId s)
               clientUpdate
               clientSend
               (MkServerState M.empty M.empty s))

     

           
