{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Monoid hiding ((<>))
import Prelude hiding ((.), id, null, filter)
import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL as SDL
import Linear.V2
import Control.Lens

import Debug.Trace
deriving instance Ord SDL.Keysym


--- Data Types
--
type Coord = (V2 Int)
-- world is 50x50 ??
data World = World {
           cell :: Coord,
           size :: Int -- x * x
           }

newWorld :: World
newWorld = World { cell = (V2 0 0)
                 , size = (800 `div` 80)
                 }
--
--- Functions
--

render :: World -> SDL.Surface -> IO ()
render world screen = do
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
          SDL.fillRect screen Nothing
        let wa = case world of
                  World {cell = x} -> gameToScreen x
        let (cellX, cellY) = case wa of
              V2 ix iy -> (ix,iy)

        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
            SDL.fillRect screen (Just $ SDL.Rect cellX cellY 50 50)

        SDL.flip screen
        where
          gameToScreen v2 = v2 * (V2 10 10)

gameLoop :: Set SDL.Keysym -> SDL.Surface -> Session IO s ->
            Wire s e IO (Set SDL.Keysym) World -> World -> IO ()
gameLoop keysDown screen s w x = do
    keysDown' <- parseEvents keysDown
    (ds, s') <- stepSession s
    (ex, w') <- stepWire w ds (Right keysDown')
    case ex of
      Right world -> do
        render world screen
        gameLoop keysDown' screen s' w' world
      Left _ -> return ()

velocity :: (Monad m, Monoid e) => Wire e () m (Set SDL.Keysym) Coord
velocity =  pure (V2 (-1) 0)  . when (keyDown SDL.SDLK_LEFT)
        <|> pure (V2 1 0)   . when (keyDown SDL.SDLK_RIGHT)
        <|> pure (V2 0 (-1))   . when (keyDown SDL.SDLK_UP)
        <|> pure (V2 0 1)  . when (keyDown SDL.SDLK_DOWN)
        <|> pure (V2 0 0)

position :: (Monoid s) => Coord -> Wire s () m Coord Coord
position x' = mkPure $ \ds dx ->
                x' `seq` (Right x', position (x' + dx))

cellPos :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) Coord
cellPos = proc keysDown -> do
  speed <- velocity -< keysDown
  pos <- position (V2 1 1) -< speed
  returnA -< pos

gameFrame :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) World
gameFrame = proc keysDown -> do
  nCell <- cellPos -< keysDown
  returnA -< World { cell = nCell
                   , size = (800 `div` 80)
                   }

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 800 800 0 [SDL.HWSurface]
  -- more setup stuff?
  gameLoop empty screen clockSession_ gameFrame newWorld

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (insert k keysDown)
    SDL.KeyUp k -> parseEvents (delete k keysDown)
    _ -> parseEvents keysDown

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.symKey)
