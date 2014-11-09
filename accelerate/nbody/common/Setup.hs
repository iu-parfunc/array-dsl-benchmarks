import Distribution.Simple
import System.Process

main :: IO ()
main = do
  -- Ensure the input file is generated
  system "make -C ../points uniform.3dpts"
  defaultMain

