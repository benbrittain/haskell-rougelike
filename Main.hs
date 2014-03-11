{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Monoid hiding ((<>))
import Prelude hiding ((.), id, null, filter)
import Control.Wire hiding (empty)
import Control.Monad hiding (when)
import FRP.Netwire hiding (empty) -- has integral, currently unused
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL
import Linear.V2
import Control.Lens
import qualified Data.Map as Map

import Types
import Render

deriving instance Ord SDL.Keysym


--
--- Functions
--

gameLoop :: Set SDL.Keysym -> SDL.Surface -> Session IO s ->
            Wire s e IO (Set SDL.Keysym) World -> World -> IO ()
gameLoop keysDown screen s wire world = do
    keysDown' <- parseEvents keysDown
    (ds, s') <- stepSession s
    (world', wire') <- stepWire wire ds (Right keysDown')
    case world' of
      Right world -> do
        render world screen
        gameLoop keysDown' screen s' wire' world
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
  pos <- position (V2 0 0) -< speed
  returnA -< pos

genWorld :: (Monoid s) => World -> Wire s () m Coord World
genWorld x' = mkPure $ \ds dx ->
                x' `seq` (Right x', genWorld (modWorld dx))
              where
                modWorld ds = World { wCell = ds
                                   , wSize = (V2 25 25)
                                   , wTiles = getTiles x'
                                   }
                getTiles z = case z of
                               World {wTiles = tiles} -> tiles


gameFrame :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) World
gameFrame = proc keysDown -> do
  nCell <- cellPos -< keysDown
  nWorld <- genWorld newWorld -< (nCell)
--  nTiles <- Map.empty
  returnA -< nWorld

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
