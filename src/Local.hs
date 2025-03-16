module Main where

import SDL

import Game (runGame, windowConfig, windowConf, defaultConfig)
import FrontEnd
import Mines

import Data.Aeson
import Data.Aeson.Decoding
import Data.ByteString.Lazy as B

main :: IO ()
main =
  do decoded <- fmap eitherDecode $ B.readFile confFile 
     case decoded of
       Left s -> putStrLn s
       Right userConf ->
         (do let (w, h)    = screen_size userConf
             let mineConf  = MkMineConf (tile_dimensions userConf) (number_of_mines userConf)
             let tileWidth = tile_size userConf
     
             runGame (defaultConfig {windowConf = windowConfig {windowInitialSize = V2 1020 900}})
                     (initGameState mineConf)
                     (draw tileWidth)
                     update
                     (eventHandler tileWidth))
  
