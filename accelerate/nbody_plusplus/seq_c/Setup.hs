import Distribution.Simple
import System.Process
main = do 
  -- Make sure our input file is generated.
  putStrLn "[cabal] using custom build to generate input file."
  system "(cd ../makefile_based; make uniform.3dpts)"
  defaultMain
