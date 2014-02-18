module SPMD where

import Data.Array.Accelerate (Acc, Arrays)
import Data.Array.Accelerate.BackendKit.SPMD
import Data.Array.Accelerate.C
import System.IO.Unsafe

backends :: [CBackend]
backends = unsafePerformIO discoverBackends

discoverBackends :: IO [CBackend]
discoverBackends = mapM (const mkCBackend) [1..2]

run :: (Arrays a) => Acc a -> a
run = unsafePerformIO . runAccSPMD backends
