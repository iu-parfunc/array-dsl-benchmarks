{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (

  Config, Solver(..),
  configBackend, configSolver, configBodyCount, configBodyMass, configTimeStep,
  configEpsilon, configStartDiscSize, configStartSpeed, configMaxSteps,
  configDumpFinal,

  parseArgs,
  run,
  run1,

) where

import Common.Type

import Data.Char
import Data.List
import Data.Label
import System.Exit
import System.Console.GetOpt
import qualified Criterion.Main                         as Criterion
import qualified Criterion.Config                       as Criterion

import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interp
import qualified Data.Array.Accelerate.CUDA             as CUDA



-- | Program configuration
--
data Backend = Interpreter
             | CUDA
  deriving (Bounded, Show)


data Solver = Naive | BarnsHut
  deriving (Enum, Bounded, Show)


data Config
  = Config
  {
    -- How to execute the simulation
    _configBackend              :: Backend
  , _configSolver               :: Solver


    -- System setup
  , _configBodyCount            :: Int
  , _configBodyMass             :: R
  , _configTimeStep             :: R
  , _configEpsilon              :: R

    -- Initial conditions
  , _configStartDiscSize        :: R
  , _configStartSpeed           :: R

    -- Terminating conditions
  , _configMaxSteps             :: Maybe Int
  , _configHelp                 :: Bool

    -- Dump final particle locations to file
  , _configDumpFinal            :: Maybe FilePath
  }
  deriving Show

$(mkLabels [''Config])


defaultConfig :: Config
defaultConfig = Config
  {
    _configBackend              = maxBound
  , _configSolver               = Naive         -- no barns-hut yet!

  , _configBodyCount            = 200
  , _configBodyMass             = 1000
  , _configTimeStep             = 1
  , _configEpsilon              = 50

  , _configStartDiscSize        = 250
  , _configStartSpeed           = 0.5

  , _configMaxSteps             = Nothing
  , _configHelp                 = False

  , _configDumpFinal            = Nothing
  }


-- | Execute Accelerate expressions
--
run :: Arrays a => Config -> Acc a -> a
run config =
  case _configBackend config of
    Interpreter -> Interp.run
    CUDA        -> CUDA.run



run1 :: (Arrays a, Arrays b) => Config -> (Acc a -> Acc b) -> a -> b
run1 config f =
  case _configBackend config of
    Interpreter -> head . Interp.stream f . return
    CUDA        -> CUDA.run1 f


-- | The set of available command-line options
--
defaultOptions :: [OptDescr (Config -> Config)]
defaultOptions =
  [ Option  [] ["interpreter"]
            (NoArg (set configBackend Interpreter))
            "reference implementation (sequential)"


  , Option  [] ["cuda"]
            (NoArg (set configBackend CUDA))
            "implementation for NVIDIA GPUs (parallel)"


  , Option  ['s'] ["solver"]
            (ReqArg (set configSolver . solver) "ALGORITHM")
            ("solver to use, one of: " ++ intercalate ", " (map show [minBound .. maxBound :: Solver]))

  , Option  ['n'] ["bodies"]
            (ReqArg (set configBodyCount . read) "INT")
            (describe configBodyCount "number of bodies in the simulation")

  , Option  [] ["mass"]
            (ReqArg (set configBodyMass . read) "FLOAT")
            (describe configBodyMass "mass of each body")

  , Option  [] ["timestep"]
            (ReqArg (set configTimeStep . read) "FLOAT")
            (describe configTimeStep "time step between simulation states")

  , Option  [] ["epsilon"]
            (ReqArg (set configEpsilon . read) "FLOAT")
            (describe configEpsilon "smoothing parameter")

  , Option  [] ["disc"]
            (ReqArg (set configStartDiscSize . read) "FLOAT")
            (describe configStartDiscSize "initial size of particle disc")

  , Option  [] ["speed"]
            (ReqArg (set configStartSpeed . read) "FLOAT")
            (describe configStartSpeed "initial rotation speed of the disc")

  , Option  [] ["max-steps"]
            (ReqArg (set configMaxSteps . read) "INT")
            (describe configMaxSteps "exit simulation after this many steps")

  , Option  [] ["dump-final"]
            (ReqArg (set configDumpFinal . Just) "FILE")
            "dump final body positions to file"

  , Option  ['h', '?'] ["help"]
            (NoArg (set configHelp True))
            "show this help message"
  ]
  where
    solver algorithm
      | a `elem` ["n",  "naive"]                        = Naive
      | a `elem` ["bh", "barnshut", "barns-hut"]        = BarnsHut
      | otherwise                                       = error $ "Unknown solver method: " ++ algorithm
      where
        a = map toLower algorithm

    describe f msg
      = msg ++ " (" ++ show (get f defaultConfig) ++ ")"


-- | Process the command line options
--
parseArgs :: [String] -> IO (Config, Criterion.Config, [String])
parseArgs argv
  = let
        helpMsg err     = concat err
          ++ usageInfo header                         defaultOptions
          ++ usageInfo "\nGeneric criterion options:" Criterion.defaultOptions

        header          = unlines
          [ "accelerate-nbody (c) [2012] The Accelerate Team"
          , ""
          , "Usage: accelerate-nbody [OPTIONS]"
          ]

  in case getOpt' Permute defaultOptions argv of
      (o,_,n,[])  -> do

        -- pass unrecognised options to criterion
        (cconf, rest)     <- Criterion.parseArgs Criterion.defaultConfig Criterion.defaultOptions n
        let conf = foldr id defaultConfig o
        return (conf, cconf, rest)

      (_,_,_,err) -> error (helpMsg err)

