import Distribution.Simple
import System.Process
main = do 
  -- Make sure our input file is generated.
  system "(cd ../makefile_based; make uniform.3dpts)"
  defaultMain
