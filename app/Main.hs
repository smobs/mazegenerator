module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Data.Set (Set(..))
import Data.Monoid ((<>))
import qualified Data.Set as S
import System.Random

import Lib

type Square = (Int, Int)

type Model = Set Square

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
    S.fromList
      [ (x, y)
      | x <- [0 .. (mazeSize - 1)]
      , y <- [0 .. (mazeSize - 1)]
      , xMask !! x == 0
      , yMask !! y == 0 ]

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
    10
    init
    draw
    step

draw :: Model -> Picture
draw w = foldMap f w
  where
    f (x, y) = translate (translateToScreen x) (translateToScreen y) drawSquare

translateToScreen :: Int -> Float
translateToScreen x = scaleF * (fromIntegral x) - (screenSize / 2)

scaleF
  :: Fractional a
  => a
scaleF = screenSize / mazeSize

drawSquare :: Picture
drawSquare =
  let size = scaleF * 1
  in translate (size / 2) (size / 2) $ color black $ rectangleSolid size size

step :: a -> b -> Model -> Model
step _ _ s = (newGrid s . withCounts s . interestingPoints) s

newGrid :: Model -> Set (Square, Int) -> Model
newGrid w i = (S.map fst . S.filter f) i
  where
    f (_, 3) = True
    f (p, n) = n > 0 && n < 5 && S.member p w

withCounts :: Model -> Set Square -> Set (Square, Int)
withCounts w = S.map f
  where
    f p = (p, length (S.filter (flip S.member w) (neighbours p)))

interestingPoints :: Set Square -> Set Square
interestingPoints w = w <> (foldMap neighbours w)

neighbours :: (Int, Int) -> Set (Int, Int)
neighbours (x, y) =
  S.fromList
    [ (x', y')
    | a <- [-1 .. 1]
    , b <- [-1 .. 1]
    , let x' = x + a
    , let y' = y + b
    , y' >= 0
    , x' >= 0
    , y' < mazeSize
    , a /= 0 || b /= 0
    , x' < mazeSize ]
