module Render where

import Graphics.UI.SDL as SDL
import Linear.V2
import Control.Lens

import Types

render :: World -> SDL.Surface -> IO ()
render world screen = do
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
          SDL.fillRect screen Nothing
        let wa = case world of
                  World {cell = x} -> gameToScreen x
        let (cellX, cellY) = case wa of
              V2 ix iy -> (ix,iy)

        mapM drawCell [(V2 x y) | x <- [0..50], y <- [0..50]]
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
            SDL.fillRect screen (Just $ SDL.Rect cellX cellY 50 50)

        SDL.flip screen
        where
          gameToScreen v2 =
            v2 * (V2 10 10)
          drawCell x = (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
            SDL.fillRect screen (Just $ SDL.Rect (coords ^._x) (coords ^._y) 10 10)
            where
              coords = gameToScreen x

