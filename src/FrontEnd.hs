{-# LANGUAGE DeriveGeneric #-}

module FrontEnd where -- this name is shit i wanted to say game but there already is a file

import SDL hiding (get)
import qualified SDL.Font as F

import Mines
import Game

import Data.List
import Data.Monoid
import Data.Maybe
import Data.Aeson

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy hiding (modify)

import GHC.Generics

data UserConf = MkUserConf {screen_size     :: (Int,Int)
                           ,number_of_mines :: Int
                           ,tile_dimensions :: (Int, Int)
                           ,tile_size       :: Int
                           ,host_name       :: String
                           ,service_name    :: String}
                deriving Generic

fromUserConf :: UserConf -> MineConf
fromUserConf (MkUserConf _ n t _ _ _) = MkMineConf t n

confFile = "conf.txt"


instance FromJSON UserConf
                
data Action = R Position | L Position
  deriving (Generic, Eq, Show)

fromButton :: Position -> SDL.MouseButton -> Maybe Action
fromButton p ButtonLeft  = Just $ L p
fromButton p ButtonRight = Just $ R p
fromButton p _           = Nothing

instance FromJSON Action
instance ToJSON   Action

type UpdateMessage = [Action]
type ActionConformationMessage = [Action]

data ServerMessage = Init Seed | Upd (UpdateMessage, ActionConformationMessage)
  deriving (Generic, Show)

data ClientMessage = Uninitialized
                   | Initialized UpdateMessage
  deriving (Generic, Show)

instance FromJSON ServerMessage
instance ToJSON   ServerMessage

instance FromJSON ClientMessage
instance ToJSON   ClientMessage

data Tile = Neighbour Int | Flagged | Hidden | Mine
  deriving Eq

data Gamestate = MkGamestate {markedTiles :: [[Tile]], mines :: [Position], textFont :: F.Font, gameOver :: Bool}

fontFilePath  = "Art/animeace.ttf"
fontPointSize = 20 :: Int

type Width = Int

draw :: Width ->  Gamestate -> Int -> GraphicalProcess
draw tileWidth (MkGamestate tiles _ font _) framecount = mconcat
                                      $ map (\(tile, pos) -> drawTile font tileWidth pos tile)
                                      $ concat
                                      $ map (\(row, y) -> zip row  [V2 x y | x <- [0..]])
                                      $ zip tiles [0..]


drawTile :: F.Font -> Width -> V2 Int -> Tile -> GraphicalProcess
drawTile font w pos (Neighbour i) = rect white       (pos*square, square) <> outline (dark white) (pos*square, square) <> number
  where square                = V2 w w
        number                = text font (numberColor i) (pos*square + V2 ((w - textWidth) `div` 2 ) ((w - textHeight) `div` 2)) (show i)
        (textWidth, textHeight) = textSize font (show i)
drawTile font w pos Flagged       = rect (dark red)  (pos*square, square) <> outline (dark white) (pos*square, square)
  where square = V2 w w
drawTile font w pos Hidden        = rect (dark blue) (pos*square, square) <> outline (dark white) (pos*square, square)
  where square = V2 w w
drawTile font w pos Mine          = rect (dark orange) (pos*square, square) <> outline (dark white) (pos*square, square)
  where square = V2 w w  

numberColor :: Int -> Color
numberColor 0 = white
numberColor 1 = blue
numberColor 2 = green
numberColor 3 = orange
numberColor 4 = purple
numberColor _ = black

  
update :: Gamestate -> Float -> Gamestate
update g f = if Mine `elem` concat (markedTiles g) then g {gameOver = True}
                                                   else g

replace :: Int -> a -> [a] -> [a]
replace i a as = take i as ++ [a] ++ drop (i+1) as

modify :: (Position -> Tile) -> Position -> [[Tile]] -> [[Tile]]
modify f (x,y) tiles = replace y (replace x (f (x, y)) (tiles!!y)) tiles


eventHandler :: Width -> Event -> Gamestate -> Gamestate
eventHandler tileWidth (Event _ event) g = case event of
                                   MouseButtonEvent a -> mouseEvent a 
                                   _                  -> g
            where mouseEvent (MouseButtonEventData _ motion _ button _ (P (V2 x' y')))
                    = let (x,y) = (fromIntegral x', fromIntegral y')
                         in if motion == Pressed && not (gameOver g)
                            then if (y <= length (markedTiles g) * tileWidth) && (x  <= length (markedTiles g !! 0) * tileWidth)
                                 then fromMaybe g (fmap (\x -> mousePressed x g) $ fromButton (x `div` tileWidth, y `div` tileWidth) button)
                                 else g
                            else g

mousePressed :: Action -> Gamestate -> Gamestate
mousePressed (L (x,y)) g = if y >= length (markedTiles g) || x >= length (markedTiles g !! 0) || x < 0 || y < 0
                              then g
                              else case (markedTiles g !! y) !! x of
                                          Neighbour a -> g
                                          _           -> let newTile = showTile (mines g) (x,y)
                                                             g'      = g {markedTiles = modify (const newTile) (x, y) (markedTiles g)}
                                                         in if newTile  == Neighbour 0
                                                            then foldr (\x -> mousePressed (L x)) g' (filter (\(i,j) -> i /= 0 || j /= 0) [(x + i, y + j) | i <- [-1,0,1], j <- [-1, 0, 1]])
                                                            else g'
mousePressed (R (x,y)) g = g {markedTiles = modify (const (case (markedTiles g !! y) !! x of
                                                                     Hidden  -> Flagged
                                                                     Flagged -> Flagged -- Hidden -- So i do this to not have the issue with blinking in online mode.
                                                                     a       -> a)) (x, y) (markedTiles g)}


showTile :: [Position] -> Position -> Tile
showTile mines pos = if pos `elem` mines
                        then Mine
                        else Neighbour $ getDegreeOfTile pos mines

initGameState :: MineConf -> RenderingContext Gamestate
initGameState mineConf =
  do mines <- liftIO (getMines mineConf)
     font  <- F.load fontFilePath fontPointSize
     return (MkGamestate [[Hidden | i <- [0..fst (dimensions mineConf)-1]]
                                  | j <- [0..snd (dimensions mineConf)-1]] mines font False)


