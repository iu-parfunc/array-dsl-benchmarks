{-# LANGUAGE OverloadedStrings #-}

module IO where

import Common.Type
import Data.Array.Accelerate                            as A

import Prelude                                          as P
import Control.Monad
import Data.Char
import Data.Maybe
import System.IO
import Text.Printf
import GHC.Float

import Data.ByteString.Char8                            ( ByteString )
import Data.ByteString.Lex.Double                       ( readDouble )
import qualified Data.ByteString.Char8                  as B


-- | Read a PBBS geometry file (3D points):
--
readGeomFile
    :: Maybe Int
    -> FilePath
    -> IO (Vector Position)
readGeomFile nMax fp = do
  str <- B.readFile fp
  let (header:points)   = B.lines str
      nIn               = P.length points
      n                 = fromMaybe nIn nMax

  -- Basic error checking
  when (n > nIn)
    $ error
    $ printf "Not enough data in file! Requested: %d, found: %d\n" n nIn

  when (header /= "pbbs_sequencePoint3d")
    $ error
    $ printf "Unexpected header line: %s\n" (B.unpack header)

  -- A simple line parser
  let parse :: Int -> ByteString -> Position
      parse i l =
        let trim = B.dropWhile isSpace
            go   = do
              (x,r1) <- readDouble l
              (y,r2) <- readDouble (trim r1)
              (z,_ ) <- readDouble (trim r2)
              return (double2Float x, double2Float y, double2Float z)
        in
        case go of
          Nothing -> error $ printf "Parse failure on line %d: %s\n" i (B.unpack l)
          Just x  -> x

  -- Read in the input data and return as an Accelerate array
  return
    $ A.fromList (Z :. n)
    $ P.zipWith parse [0..] points



-- | Write a vector of 3D positions to a PBBS geometry file
--
writeGeomFile
    :: FilePath
    -> Vector Position
    -> IO ()
writeGeomFile fp arr =
  withFile fp WriteMode $ \h -> do
    -- header
    hPutStrLn h "pbbs_sequencePoint3d"

    -- points
    forM_ (toList arr) $ \(x,y,z) -> do
      hPutStrLn h $ printf "%f %f %f" x y z

