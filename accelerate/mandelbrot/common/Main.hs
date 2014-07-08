{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
--
-- A Mandelbrot set generator.
-- Originally submitted by Simon Marlow as part of Issue #49.
--

-- import World
import qualified Mandel 
-- import Config

-- import Data.Label
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

import Data.Array.Accelerate.BackendClass (runTimed, runTimedSimple, AccTiming(..), SimpleBackend(..))
import Data.Array.Accelerate.BackendKit.CompilerPipeline (phase0, phase1)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Prelude                                  as P
import Data.Array.Accelerate                    as A hiding ( size, (++))
import System.Environment (getArgs, getEnvironment)

#ifdef ACCBACKEND
import qualified ACCBACKEND as Bkend
#else
#error "Must specify ACCBACKEND CPP variable to build this nbody benchmark."
-- import qualified Data.Array.Accelerate.CUDA as Bkend
#endif

-- Temp hack: 
#ifdef EXTRAINITCUDA
import Foreign.CUDA.Driver (initialise)
#endif



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

        -- let def_size  = 512
        --     def_depth = 255

        let def_size  = 2000
            def_depth = 25

        args <- getArgs 
        let (size,depth) = case args of
                            []   -> (def_size, def_depth)
                            [sz] -> (read sz, def_depth)
                            [sz,d] -> (read sz, read d)
        putStrLn$"Mandel running on "++show size++"x"++show size++" image size, depth "++show depth

#ifdef EXTRAINITCUDA
        initialise []
        putStrLn$"CUDA initialized - this is a hack to work around an apparent accelerate-cuda bug."
#endif

        let view  = (-0.25, -1.0, 0.0, -0.75)
            fullacc :: Acc (Array DIM2 (Mandel.Complex Float,Int))
            fullacc = Mandel.mandelbrot size size depth (A.unit (A.constant view))
        let simpl = phase1 $ phase0 fullacc
        tBegin <- getCurrentTime
#ifndef NOSIMPLE
        (times,_output) <- runTimedSimple Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig simpl
        putStrLn$ "Finished executing through SimpleBackend. "
#else
        (times,_output) <- runTimed Bkend.defaultBackend Nothing Bkend.defaultTrafoConfig fullacc
#endif
        let AccTiming{compileTime,runTime,copyTime} = times
        putStrLn$ "  All timing: "P.++ show times
        tEnd   <- getCurrentTime
-- ifndef DONTPRINT       
#if 1
        putStrLn$ "JITTIME: "P.++ show compileTime
        putStrLn$ "SELFTIMED: "P.++ show (runTime + copyTime)
#endif
        putStrLn$ "Total elapsed time: "P.++ show (diffUTCTime tEnd tBegin)

        return ()
