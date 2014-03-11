module Render where

import Graphics.UI.SDL as SDL
import Linear.V2
import Control.Lens

import Types

tileSize :: Int
tileSize = (800 `div` 25)

render :: World -> SDL.Surface -> IO ()
render world screen = do
    clear screen

    drawTiles screen

    let (cell) = case world of
                World {wCell = cell} -> (cell)
    drawCell screen $ gameToScreen cell

    SDL.flip screen

    where
      clear screen =
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
          SDL.fillRect screen Nothing
      drawTiles screen =
        mapM (drawTile screen Empty) [gameToScreen (V2 x y) | x <- [0..25], y <- [0..25]]
      gameToScreen v2 =
        v2 * (V2 tileSize tileSize)

drawTile :: SDL.Surface -> Tile -> Coord -> IO Bool
drawTile screen tile coord=
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 211 211 211 >>=
      SDL.fillRect screen (Just $ SDL.Rect (coord ^._x) (coord ^._y) tileSize tileSize )

drawCell :: SDL.Surface -> Coord -> IO Bool
drawCell screen coord =
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
      SDL.fillRect screen (Just $ SDL.Rect (coord ^._x) (coord ^._y) tileSize tileSize )

