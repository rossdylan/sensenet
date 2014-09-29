{-# LANGUAGE OverloadedStrings #-}
module Main where

--import System.ZMQ4.Monadic
import Control.Monad (forever)
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map.Strict as Map
import Data.Aeson
import Control.Applicative
import Control.Monad
-- alias for zmq uris
type ZmqUri = String

-- sensor data used to transport data over the network
data SensorData = SensorData {
    hostName :: String, 
    sData :: Float }

instance FromJSON SensorData where
    parseJSON (Object v) = SensorData <$>
                            v .: "hostname" <*>
                            v .: "data"
    parseJSON _ = mzero

instance ToJSON SensorData where
    toJSON (SensorData hname sdata) = object ["hostname" .= hname, "data" .= sdata]

-- map hostnames to a list of floats
type SensorMap = Map.Map String [Float]

-- STM Wrapper
type GlobalMap = TVar SensorMap

-- Helper function to atomically read a TVar
atomRead = atomically . readTVar

-- Function that adds some sensor data to the global sensor data map
mapUpdate :: SensorData -> SensorMap -> SensorMap
mapUpdate sd m = Map.update (\val-> Just (sData sd : val)) (hostName sd) m


-- Atomically add some data to the global data map
addSensorData :: GlobalMap -> SensorData -> IO ()
addSensorData gd sd = do
    globular <- atomRead gd
    atomically $ writeTVar gd $ mapUpdate sd globular

-- Atomically get all sensor data for a hostname
getSensorData :: GlobalMap -> String -> IO (Maybe [Float])
getSensorData gd hostname = do 
    globular <- atomRead gd
    globular >>= Map.lookup hostname
-- Thread to listen for peeps over the network

{-
remoteListener :: GlobalMap -> ZmqUri -> IO ()
remoteListener gd uri = runZMQ $ do
    pub <- socket Pub
    bind pub uri
    forever $ do
        line <- liftIO $ fromString <$> getLine
        

-}
main = return ()


