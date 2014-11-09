import Distribution.Simple
import System.Process

main :: IO ()
main = do
  -- Ensure the input files are generated
  system "make -C ../points"
  defaultMain

