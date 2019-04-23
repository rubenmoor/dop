module Main where

import           Control.Monad               (foldM)
import           Data.Text.IO                (putStrLn)
import           Data.Vector.Unboxed.Mutable (IOVector, clone, modify, read,
                                              replicate, write, length)
import           Options.Applicative         (header, progDesc, fullDesc, execParser, helper, info, (<**>))
import           Prelude                     ((<>), Bool (..), Double, IO, Int, mod,
                                              pure, undefined, ($), (*), (+),
                                              (-), (.), (<), (<$), (==), (>),
                                              (||))
import           System.Random               (randomIO)

import           Options                     (opts, Opts (..))

type CellState = Bool

type NNeighbors = IOVector Int
type CellStates = IOVector CellState
-- Grid length along one dimension, cells, and number of neighbors
type Grid = (Int, CellStates, NNeighbors)

type Cell = (Int, CellState)
type GridUpdate = [Cell]

neighborIndices :: Int -> Int -> [Int]
neighborIndices gridWidth i =
  let length = gridWidth * gridWidth
      leftMargin = if i `mod` gridWidth == 0 then gridWidth else 0
      rightMargin = if i + 1 `mod` gridWidth == 0 then gridWidth else 0
      n  = (i - gridWidth + length) `mod` length
      nw = (n - 1 + leftMargin) `mod` length
      ne = (n + 1 - rightMargin + length) `mod` length
      w  = (i - 1 + leftMargin) `mod` length
      e  = (i + 1 - rightMargin + length) `mod` length
      s  = (i + gridWidth) `mod` length
      sw = (s - 1 + leftMargin) `mod` length
      se = (s + 1 - rightMargin + length) `mod` length
  in  [nw, n, ne, w, e, sw, s, se]

gridSet :: Grid -> Int -> CellState -> IO ()
gridSet (gridWidth, cellStates, newNNeighbors) i newState = do
    foldM (\_ -> modify newNNeighbors f) () (neighborIndices gridWidth i)
    write cellStates i newState
  where
    f n = if newState then n + 1 else n - 1

nextState :: Grid -> IO (GridUpdate, Grid)
nextState (gridWidth, cellStates, oldNNeighbors) = do
    newNNeighbors <- clone oldNNeighbors
    upd <- forLoop [] 0 (gridWidth * gridWidth) $ \update i -> do
        n <- read oldNNeighbors i
        cState <- read cellStates i
        if cState
          then if n < 2 || n > 3
                 then change newNNeighbors i False update
                 else pure update
          else if n == 3
                 then change newNNeighbors i True update
                 else pure update
    pure $ (upd, (gridWidth, cellStates, newNNeighbors))
  where
    change :: NNeighbors -> Int -> Bool -> GridUpdate -> IO GridUpdate
    change nNeighbors i b update = do
      gridSet (gridWidth, cellStates, nNeighbors) i b
      pure $ (i, b) : update

-- todo: how efficient is this?
forLoop :: GridUpdate -> Int -> Int -> (GridUpdate -> Int -> IO GridUpdate) -> IO GridUpdate
forLoop upd i n _ | i == n = pure upd
forLoop upd i n func = do
  upd' <- func upd i
  forLoop upd' (i + 1) n func

randomGrid :: Int -> IO (GridUpdate, Grid)
randomGrid gridWidth = do
  cellStates <- replicate (gridWidth * gridWidth) False
  nNeighbors <- replicate (gridWidth * gridWidth) (0 :: Int)
  update <- forLoop [] 0 (gridWidth * gridWidth) $ \update i -> do
    r <- randomIO
    if (r :: Double) < 0.2
      then do
        gridSet (gridWidth, cellStates, nNeighbors) i True
        pure $ (i, True) : update
      else pure update
  pure $ (update, (gridWidth, cellStates, nNeighbors))

main :: IO ()
main = do
  let myInfo = info (opts <**> helper)
        ( fullDesc
        <> progDesc "CGOL performance test"
        <> header "run CGOL iterations using efficient vectors"
        )
  Opts {..} <- execParser myInfo
  grid <- randomGrid optSize
  putStrLn "Hu"
