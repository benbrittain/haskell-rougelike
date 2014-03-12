module Types where
import Linear.V2
import Linear.V3
import qualified Data.Map as Map
import System.Random

type Coord = (V2 Int)

data Tile = Empty
          | Occupied
          | Wall
          deriving (Show, Eq)

data World = World { wCell   :: Coord
                   , wSize   :: Coord
                   , wTiles  :: Map.Map Coord Tile
                   } deriving (Show)

data Cell = Cell { cColor :: (V3 Int)
                 , cFood :: Int
                 , cPos :: Coord
                 }

-- Experimenting with CA
--initMap :: (V2 Int) -> Map.Map Coord Tile
--initMap coord = Map.insert (V2 1 1) $ 
--Map.fromList [(V2 x y, Wall) | x <- [0..3], y <- [0..3]]-- con = [(0, 0), (1, 0), (0, 1), (3, 3), (2, 3), (3, 2)]

tmap 0 = Map.insert (V2 10 10) Wall Map.empty
tmap n = step 30 $ tmap (n - 1)


neighbours :: Coord -> Map.Map Coord Tile -> [Coord]
neighbours (V2 x y) world = filter (\x -> Just Wall == Map.lookup x world)
                              [(V2 (x+dx) (y+dy)) | dx <- [-1..1], dy <- [-1..1]]

step :: Int -> Map.Map Coord Tile -> Map.Map Coord Tile
step size world = foldl (\acc x ->
                        if (((length $ neighbours x world) <= 5) && ((length $ neighbours x world) >= 1))
                          then Map.insert x Wall acc
                          else acc) Map.empty [(V2 dx dy) | dx <- [0..size], dy <- [0..size]]

-- Constructors - to be moved
newWorld :: World
newWorld = World { wCell = (V2 0 0)
                 , wSize = (V2 25 25)
                 , wTiles = tmap 41
                 }

createScreen :: Map.Map Coord Tile
createScreen =  Map.insert (V2 1 2) Wall $
                Map.insert (V2 1 3) Wall $
                Map.insert (V2 1 1) Wall $ generateMap Map.empty


--generateMap :: Map.Map Coord Tile -> Map.Map Coord Tile
generateMap chamber
      | Map.null chamber = generateMap $ insertWall Map.empty
--      | Map.null chamber = insertWall Map.empty
      | otherwise = chamber
      where
        insertWall chamber = if (head $ colOrRow)
                               then foldl (\acc x -> Map.insert (V2 x (head random)) Wall acc) chamber [0..24]
                               else foldl (\acc x -> Map.insert (V2 (head random) x) Wall acc) chamber [0..24]
        random =
          randomRs (1, 25) (mkStdGen 8) :: [Int]
        colOrRow =
          randomRs (False, True) (mkStdGen (-500)) :: [Bool]

x = generateMap Map.empty
--generateMap $ insertWall chamber
