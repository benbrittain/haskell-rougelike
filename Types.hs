module Types where
import Linear.V2
-- Types
type Coord = (V2 Int)
-- world is 50x50 ??
data World = World {
           cell :: Coord,
           size :: Int -- x * x
           }
