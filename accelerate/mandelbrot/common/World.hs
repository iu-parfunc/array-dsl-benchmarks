{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module World (

  -- Types
  World,

  -- Updating the World state
  renderWorld, initialWorld, refocus

  ) where

import Mandel
import Config

import Data.Char
import Data.Label
import Data.Array.Accelerate                    as A hiding ( size )


-- World state
-- -----------

data Zoom       = In  | Out
data Move       = Fwd | Rev

data Precision  = Float | Double

data World where
  World :: (Elt a, RealFloat a)
        => View a
        -> Render a
        -> Maybe Zoom
        -> Maybe Move   -- horizontal movement
        -> Maybe Move   -- vertical movement
        -> World


-- Render the picture
--
renderWorld :: World -> Bitmap
renderWorld (World view render _ _ _) = render $ A.fromList Z [view]


-- Initialise the World state
--
initialWorld :: Options -> View Float -> World
initialWorld config view
  = setPrecisionOfWorld Float config
  $ World view undefined Nothing Nothing Nothing


-- Reset the rendering routines to compute with the specified precision
--
setPrecisionOfWorld :: Precision -> Options -> World -> World
setPrecisionOfWorld f config (World p _ z h v)
  = let
        size    = get optSize config
        limit   = get optLimit config

        render :: (Elt a, IsFloating a) => Render a
        render  = run1 config
                $ A.map (prettyRGBA (constant limit))
                . mandelbrot size size limit

    in case f of
         Float  -> World (convertView p :: View Float)  render z h v
         Double -> World (convertView p :: View Double) render z h v


-- Event handling
-- --------------

-- Refocus the viewport by adjusting the limits of the x- and y- range of the
-- display, based on the current key state.
--
refocus :: World -> World
refocus = move . zoom
  where
    -- translate the display
    --
    move :: World -> World
    move world@(World viewport r z h v)
      = World (translate (dy,dx) viewport) r z h v
      where
        dx = case get horizontal world of
               Nothing   ->  0
               Just Fwd  ->  0.025
               Just Rev  -> -0.025

        dy = case get vertical world of
               Nothing   ->  0
               Just Fwd  ->  0.025
               Just Rev  -> -0.025

        translate (i,j) (x,y,x',y') =
          let sizex = x' - x
              sizey = y' - y
          in (x+i*sizex, y+j*sizey, x'+i*sizex, y'+j*sizey)

    -- zoom the display in or out
    --
    zoom :: World -> World
    zoom world@(World viewport r z h v)
      = World (scale s viewport) r z h v
      where
        s = case get zooming world of
              Nothing   -> 1
              Just In   -> 0.975
              Just Out  -> 1.025

        scale alpha (x,y,x',y') =
          let dx    = sizex * alpha / 2
              dy    = sizey * alpha / 2
              sizex = x' - x
              sizey = y' - y
              midx  = x + sizex / 2
              midy  = y + sizey / 2
          in (midx - dx, midy - dy, midx + dx, midy + dy)


-- Miscellaneous
-- -------------

zooming :: World :-> Maybe Zoom
zooming = lens (\(World _ _ z _ _)   -> z) (\z (World p r _ h v) -> World p r z h v)

horizontal :: World :-> Maybe Move
horizontal = lens (\(World _ _ _ h _)   -> h) (\h (World p r z _ v) -> World p r z h v)

vertical :: World :-> Maybe Move
vertical = lens (\(World _ _ _ _ v)   -> v) (\v (World p r z h _) -> World p r z h v)

convertView :: (Real a, Fractional b) => View a -> View b
convertView (x,y,x',y') = (realToFrac x, realToFrac y, realToFrac x', realToFrac y')

