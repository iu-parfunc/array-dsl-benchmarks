import Distribution.Simple
import System.Process
main = do 
  -- Make sure our input file is generated.
  system "(cd ../common; make uniform.3dpts)"
  defaultMain
