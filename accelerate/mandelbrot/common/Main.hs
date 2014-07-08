--
-- A Mandelbrot set generator.
-- Originally submitted by Simon Marlow as part of Issue #49.
--

-- import World
import qualified Mandel 
-- import Config

import Data.Label
import Control.Monad
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe
import System.Environment                       ( getArgs, withArgs )
-- import Criterion                                ( bench, whnf, runBenchmark )
-- import Criterion.Monad                          ( withConfig )
-- import Criterion.Analysis                       ( analyseMean )
-- import Criterion.Environment                    ( measureEnvironment )
import Data.Array.Accelerate.Array.Data         ( ptrsOfArrayData )
import Data.Array.Accelerate.Array.Sugar        ( Array(..) )

import Prelude                                  as P
import Data.Array.Accelerate                    as A hiding ( size )


-- Main ------------------------------------------------------------------------
main :: IO ()
main
  = do
        -- (config, critConf, nops) <- processArgs =<< getArgs
        -- let world       = initialWorld config view

        --     view        = (-0.25, -1.0, 0.0, -0.75)
        --     force arr   = indexArray arr (Z:.0:.0) `seq` arr

        --     mandel      = do
        --       mean <- withConfig critConf $ measureEnvironment >>= 
        --               flip runBenchmark (whnf (force . renderWorld) world) >>= 
        --               flip analyseMean 100
        --       putStrLn $ "SELFTIMED: " ++ show mean

        -- unless (P.null nops) $
        --   putStrLn $ "Warning: unrecognized options: " ++ show nops

        -- mandel
        return ()
