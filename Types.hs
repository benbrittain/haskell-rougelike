module Types where
import Linear.V2
import qualified Data.Map as Map

type Coord = (V2 Int)

data Tile = Empty
          | Occupied
          | Wall
          deriving (Show, Eq)

data World = World { wCell   :: Coord
                   , wSize   :: Coord
                   , wTiles  :: Map.Map Coord Tile
                   } deriving (Show)



-- Constructors

newWorld :: World
newWorld = World { wCell = (V2 0 0)
                 , wSize = (V2 25 25)
                 , wTiles = createScreen
                 }

createScreen :: Map.Map Coord Tile
createScreen = Map.insert (V2 1 1) Wall $ Map.empty
