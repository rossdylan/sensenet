import qualified Data.Set as Set


class LWWSet s where
    add     :: Eq a => s -> a -> s
    remove  :: Eq a => s -> a -> s
    exists  :: Eq a => s -> a -> Bool
    merge   :: s -> s -> s
    asList  :: Eq a => s -> [a]

type Hostname = String
type SensorValue = Float

data SensorEntry = SensorEntry Hostname SensorValue 
-- data SensorSet = Set.Set SensorEntry

main = do return ()
