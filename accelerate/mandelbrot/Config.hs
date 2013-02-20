{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (

  Options, optBackend, optLimit, optSize,
  processArgs, run, run1

) where

import qualified Criterion.Main                         as Crit
import qualified Criterion.Config                       as Crit
import Data.Label
import System.Exit
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(Permute), getOpt', usageInfo)
import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interp
import qualified Data.Array.Accelerate.CUDA             as CUDA

data Backend = Interpreter
             | CUDA

  deriving (Bounded, Show)

data Options = Options
  {
    _optBackend         :: Backend
  , _optSize            :: Int
  , _optLimit           :: Int
  , _optHelp            :: Bool
  }
  deriving Show

$(mkLabels [''Options])

defaultOptions :: Options
defaultOptions = Options
  { _optBackend         = CUDA
  , _optLimit           = 255
  , _optSize            = 512
  , _optHelp            = False
  }


run :: Arrays a => Options -> Acc a -> a
run opts = case _optBackend opts of
  Interpreter   -> Interp.run
  CUDA          -> CUDA.run

run1 :: (Arrays a, Arrays b) => Options -> (Acc a -> Acc b) -> a -> b
run1 opts f = case _optBackend opts of
  Interpreter   -> head . Interp.stream f . return
  CUDA          -> CUDA.run1 f


options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["interpreter"] (NoArg  (set optBackend Interpreter))   "reference implementation (sequential)"
  , Option []   ["cuda"]        (NoArg  (set optBackend CUDA))          "implementation for NVIDIA GPUs (parallel)"
  , Option []   ["size"]        (ReqArg (set optSize . read) "INT")     "Size of square image"
  , Option []   ["limit"]       (ReqArg (set optLimit . read) "INT")    "iteration limit for escape (255)"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))             "show help message"
  ]


-- | Two levels of argument parsing -- ours and criterions.
processArgs :: [String] -> IO (Options, Crit.Config, [String])
processArgs argv =
  case getOpt' Permute options argv of
    (o,_,n,[])  -> do -- Pass unrecognized options onward:
                      (critConf,rst) <- Crit.parseArgs Crit.defaultConfig Crit.defaultOptions n
                      let opts = foldl (flip id) defaultOptions o
                      return (opts, critConf, "--help":rst)

    (_,_,_,err) -> error (helpMsg err)
  where
    helpMsg err = concat err ++ usageInfo header options ++
                  usageInfo "\nGeneric criterion options:" Crit.defaultOptions
    header      = unlines
      [ "accelerate-mandelbrot (c) [2011..2012] The Accelerate Team"
      , ""
      , "Usage: accelerate-mandelbrot [OPTIONS]"
      , ""
      , "Translate the display using the arrow keys, zoom with 'z' and 'x'."
      , "Switch between calculation using float or double with 'f' and 'd'."
      ]
