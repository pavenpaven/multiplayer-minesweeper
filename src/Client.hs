{-# LANGUAGE BangPatterns #-} -- , BlockArguments

module Main where

import SDL
import qualified SDL.Font as F

import Game
import NetworkGame
import Buffer
import FrontEnd
import Mines

import Data.Aeson
import Data.Aeson.Decoding
import Data.Monoid
import Data.Maybe
import Data.Function
import qualified Data.ByteString.Lazy as B

import Control.Arrow

import Debug.Trace

data NetworkedMines =
  MkNetworkedMines {acts :: [Action]
                   ,game :: Maybe Gamestate
                   ,sendActs :: [Action]
                   ,ntextFont :: F.Font} -- this has to be here because i need to initialize the game without an frontend game 
  -- Actions are for things preformed by other players 
  -- I'm going to have to send conformations from servers with current disign
  -- It's not really what I want with the library but to not do this would maybe need
  -- like change in types maybe adding a fancy monad.

drawGame :: Width -> NetworkedMines -> Int -> GraphicalProcess
drawGame width netgame i = fromMaybe mempty $ fmap (\x -> draw width x i) $ game netgame

updateGame :: NetworkedMines -> Float -> NetworkedMines
updateGame n f = (fromMaybe n (fmap (\g -> n {game = Just $ localUpdate $ multiplayerActions g}) $ game n)) {acts = []}
  where localUpdate = flip update f

        multiplayerActions g = foldr mousePressed g (acts n)

events :: Width -> Event -> NetworkedMines -> NetworkedMines
events tileWidth (Event _ event) n =
  case game n of
    Nothing -> n
    Just g  -> case event of
                 MouseButtonEvent a -> let (g', mButtonAction) = mouseEvent a g
                                       in fromMaybe n
                                            (fmap (\x -> n {game = Just g', sendActs =  x : sendActs n}) mButtonAction)
                 _                   -> n
      where mouseEvent :: MouseButtonEventData -> Gamestate -> (Gamestate, Maybe Action)
            mouseEvent (MouseButtonEventData _ motion _ button _ (P (V2 x' y'))) g
              = let (x, y) = (fromIntegral x', fromIntegral y')
                in if motion == Pressed && not (gameOver g)
                   then if (y <= length (markedTiles g) * tileWidth) && (x  <= length (markedTiles g !! 0) * tileWidth)
                        then let buttonAction = fromButton (x `div` tileWidth, y `div` tileWidth) button
                               in (fromMaybe g (fmap (\x -> mousePressed x g) buttonAction)
                                 , buttonAction)
                        else (g, Nothing)
                   else (g, Nothing)
                         

getInitialGame :: RenderingContext NetworkedMines
getInitialGame =
  do font  <- F.load fontFilePath fontPointSize
     return (MkNetworkedMines [] Nothing [] font)

updateOnRecv :: MineConf -> MilliSeconds -> ServerMessage -> NetworkedMines -> NetworkedMines
updateOnRecv mineConf _ (Init seed) n                      = let mines = traceShowId $ getRandomMines mineConf seed
                                                                 startingBoard = [[Hidden | i <- [0..fst (dimensions mineConf)-1]]
                                                                                          | j <- [0..snd (dimensions mineConf)-1]]
                                                             in MkNetworkedMines [] (Just $ MkGamestate startingBoard mines (ntextFont n) False) [] (ntextFont n)
updateOnRecv mineConf _ (Upd (updMes, conformationsMes)) n = n {acts = acts n ++ updMes
                                                               ,sendActs = filter (`notElem` conformationsMes) $ sendActs n}

messageSend :: NetworkedMines -> ClientMessage
messageSend n = traceShowId (case game n of
                  Just _  -> Initialized $ sendActs n
                  Nothing -> Uninitialized)



main :: IO ()
main = 
   do decoded <- fmap eitherDecode $ B.readFile confFile 
      case decoded of
        Left s -> putStrLn s
        Right userConf ->
          (do let (w, h)    = screen_size userConf
              let tileWidth = tile_size userConf
              let mineConf  = fromUserConf userConf
              let host      = host_name userconf
             -- runGame (defaultConfig {windowConf = windowConfig {windowInitialSize = V2 1020 900}})
             --         (initGameState mineConf)
             --         (draw tileWidth)
             --         update
             --         (eventHandler tileWidth)
              runGameClientTCP (defaultNetConf
                                {hostName = Just host
                                ,gameconf = defaultConfig {windowConf = windowConfig {windowInitialSize = V2 1020 900}}})
                               updateGame
                               (updateOnRecv mineConf)
                               messageSend
                               getInitialGame
                               (drawGame tileWidth)
                               (events   tileWidth))

