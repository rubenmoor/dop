module Options where

import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, auto, help, long, metavar, option,
                                      showDefault, value)
import           Prelude             (Double, Int, (<$>), (<*>))

data Opts = Opts
  { optSize       :: Int
  , optIterations :: Int
  , optDensity    :: Double
  }

opts :: Parser Opts
opts = Opts
  <$> option auto
      (  long "size"
      <> help "Grid size along one dimension, will be squared"
      <> showDefault
      <> value 1000
      <> metavar "SIZE"
      )
  <*> option auto
      (  long "iterations"
      <> help "number of life iterations"
      <> showDefault
      <> value 100
      <> metavar "ITERATIONS"
      )
  <*> option auto
      (  long "density"
      <> help "density for the population of the initial random grid"
      <> showDefault
      <> value 0.2
      <> metavar "DENSITY"
      )
