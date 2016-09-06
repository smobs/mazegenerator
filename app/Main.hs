module Main where

import           Data.List                              (partition)
import           Data.Map                               (Map)
import qualified Data.Map.Strict                        as M
import           Data.Monoid                            ((<>))
import           Data.Set                               (Set (..))
import qualified Data.Set                               as S
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate
import           System.Random


import           Lib

data Tile = Empty | Wall | Start | Finish | Water deriving (Eq, Show)

type Square = (Int, Int)

type Model = Map Square Tile

screenSize
  :: Num a
  => a
screenSize = 500

mazeSize
  :: Num a
  => a
mazeSize = 100

initialModel :: IO Model
initialModel = do
  xMask <- mask 1
  yMask <- mask 2
  return $
    M.insert (1, 1) Start $
    M.insert (mazeSize - 1, mazeSize - 1) Finish $
    M.fromList
      [ ((x, y), Wall)
      | (x, y) <- allSquares
      , xMask !! x == 0
      , yMask !! y == 0 ]


allSquares :: [(Int, Int)]
allSquares =
  [ (x, y)
  | x <- [0 .. (mazeSize - 1)]
  , y <- [0 .. (mazeSize - 1)] ]

mask :: Int -> IO [Int]
mask i = do
  g <- getStdGen
  return $ randomRs (0, i) g

main :: IO ()
main = do
  init <- initialModel
  simulate
    (InWindow "Maze" (screenSize, screenSize) (10, 10))
    white
    3
    init
    draw
    step

draw :: Model -> Picture
draw w = foldMap f (M.toList w)
  where
    f ((x, y), t) = translate (translateToScreen x) (translateToScreen y) (drawTile t)

translateToScreen :: Int -> Float
translateToScreen x = scaleF * (fromIntegral x) - (screenSize / 2)

scaleF
  :: Fractional a
  => a
scaleF = screenSize / mazeSize

drawTile :: Tile -> Picture
drawTile t =
  let size = scaleF * 1
  in let c =
           case t of
             Wall -> black
             Finish -> red
             Start -> blue
     in if (t == Empty)
          then mempty
          else translate (size / 2) (size / 2) $ color c $ rectangleSolid size size

step :: a -> b -> Model -> Model
step _ _ s =  M.fromList $ filter (\(_,t) -> t /= Empty) $ map (\p -> (p, nextState (lookupTile p s) (neighbours p s))) allSquares

nextState :: Tile -> ([Tile],[Tile]) -> Tile
nextState t (ss, ds) =
  case t of
    Empty ->
      if nc == 3
        then Wall
        else Empty
    Wall ->
      if nc < 6 && nc > 0
        then Wall
        else Empty
    x -> x
  where
    nc = length (filter (\t -> t == Wall) (ss ++ ds))


lookupTile :: Square -> Model -> Tile
lookupTile = M.findWithDefault Empty

neighbours :: Square -> Model -> ([Tile], [Tile])
neighbours (x, y) m =
  let ds =
        [ (a, b)
        | a <- [-1 .. 1]
        , b <- [-1 .. 1]
        , a /= 0 || b /= 0 ]
  in let f ds' =
           [ lookupTile (x', y') m
           | (a, b) <- ds'
           , let x' = x + a
           , let y' = y + b
           , y' >= 0
           , x' >= 0
           , y' < mazeSize
           , x' < mazeSize ]
     in let (as, bs) = partition (\(a, b) -> abs (a + b) == 1) ds
        in (f as, f bs)
