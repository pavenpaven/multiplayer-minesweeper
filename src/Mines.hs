module Mines where

import Data.List

import System.Random

data MineConf = MkMineConf {dimensions :: (Int, Int)
                           ,numMines   :: Int}

type Position = (Int, Int)

remove :: Int -> [a] -> [a]
remove i xs = take i xs ++ drop (i + 1) xs

choice :: RandomGen b => b -> [a] -> (b, a, [a])
choice b l = let (i, b') = roughlyUniform b (length l) in (b', l !! i, remove i l)
  where roughlyUniform :: RandomGen b => b -> Int -> (Int, b)
        roughlyUniform b bound = let (i, b') = next b in (i `mod` bound, b')


type Seed = Int

getRandomMines :: MineConf -> Seed -> [Position]
getRandomMines (MkMineConf (width, height) num) seed = acc gen num possiblePos []
  where gen = mkStdGen seed
        possiblePos = filter (\x -> not (x `elem` [((width `div` 2) + i, (height `div` 2) + j) | i <- [-2 .. 2]
                                                                                                ,j <- [-2 .. 2]]))
                             [(i, j) | i <- [0 .. width - 1], j <- [0 .. height - 1]]
        acc :: RandomGen b => b -> Int -> [Position] -> [Position] -> [Position]
        acc _    0 _  xs = xs
        acc _    _ [] xs = xs
        acc seed n l  xs = let (seed', x, l') = choice seed l in acc seed' (n-1) l' (x : xs)

getMines :: MineConf -> IO [Position]
getMines conf = fmap (getRandomMines conf . fst . next) getStdGen

getDegreeOfTile :: Position -> [Position] -> Int
getDegreeOfTile (x, y) mines = sum $ map (\u -> if u `elem` mines then 1 else 0) [(x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1]]
