module Render where

import Graphics.UI.SDL as SDL
import Linear.V2
import Data.Map as Map
import Control.Lens

import Types

tileSize :: Int
tileSize = (800 `div` 25)

render :: World -> SDL.Surface -> IO ()
render world screen = do
    clear screen
    -- load map ... gotta be a more elegant way...
    let (cell, tiles) = case world of
                          World { wCell   = cell
                                , wTiles  = tiles
                                } -> (cell, tiles)

    drawTiles screen tiles

    drawCell screen $ gameToScreen cell

    SDL.flip screen

    where
      clear screen =
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
          SDL.fillRect screen Nothing
      drawTiles screen tiles =
        mapM (drawTile screen) $ (Prelude.map (\x ->
             case Map.lookup x tiles of
               Nothing    -> (Empty,  gameToScreen x)
               Just Wall  -> (Wall,   gameToScreen x)) [(V2 x y) | x <- [0..25], y <- [0..25]])
      gameToScreen v2 =
        v2 * (V2 tileSize tileSize)

drawTile :: SDL.Surface -> (Tile, Coord) -> IO Bool
drawTile screen (tileType, coord) =
    getColor screen tileType >>=
      SDL.fillRect screen (Just $ SDL.Rect (coord ^._x) (coord ^._y) tileSize tileSize)

drawCell :: SDL.Surface -> Coord -> IO Bool
drawCell screen coord =
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
      SDL.fillRect screen (Just $ SDL.Rect (coord ^._x) (coord ^._y) tileSize tileSize )

getColor :: SDL.Surface -> Tile -> IO Pixel
getColor screen Wall = (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 0
getColor screen Empty = (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 211 211 211
