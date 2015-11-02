{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Streaming.Network (HostPreference)
import           Data.String
import           Data.Time
import           Data.Word
import           Database.MySQL.Simple
import           GHC.Generics
import           Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setHost)
import           System.Environment
import qualified Web.Spock.Safe as Spock
import           Web.Spock.Safe hiding (head)

import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

data DBConfiguration = DBConfiguration
  { dbHost     :: String
  , dbPort     :: Word16
  , dbUser     :: String
  , dbPassword :: String
  } deriving Generic
instance FromJSON DBConfiguration where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = lowerFirst . drop 2 }

instance FromJSON HostPreference where
  parseJSON = fmap fromString . parseJSON

data AppConfiguration = AppConfiguration
  { appHost :: HostPreference
  , appPort :: Int
  } deriving Generic
instance FromJSON AppConfiguration where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = lowerFirst . drop 3 }

data Configuration = Configuration
  { confDB  :: DBConfiguration
  , confApp :: AppConfiguration
  } deriving Generic
instance FromJSON Configuration where
  parseJSON = genericParseJSON defaultOptions
                { fieldLabelModifier = drop 4 }

-- вообще это не обязательно, можно в ApiResponse использовать Int. Но поскольку
-- мы используем только 200 и 500 - то так круче, ничего левого нельзя передать
data ApiResponseStatus = ApiSuccess | ApiFail
  deriving Show
instance ToJSON ApiResponseStatus where
  toJSON ApiSuccess = Number 200
  toJSON ApiFail    = Number 500

data ApiResponse = ApiResponse
  { apiRespStatus :: ApiResponseStatus
  , apiRespData   :: String
  } deriving (Generic, Show)

instance ToJSON ApiResponse where
  toJSON = genericToJSON defaultOptions
             { fieldLabelModifier =  lowerFirst . drop 7 }
lowerFirst (x:xs) = toLower x : xs


type AppAction a = SpockAction Connection () () a

selectNow :: (Monad m, HasSpock m, SpockConn m ~ Connection) => m UTCTime
selectNow = do
  [Only now] <- runQuery $ \conn -> query_ conn "SELECT NOW()"
  return now

actionDB :: AppAction ()
actionDB = do
  now <- selectNow
  Spock.json $ ApiResponse ApiSuccess $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now

app = do
  get root  $ Spock.json . ApiResponse ApiFail $ "hello!"
  get (root <//> var) $ Spock.json . ApiResponse ApiSuccess
  get "db" actionDB

connBuilder :: DBConfiguration -> ConnBuilder Connection
connBuilder DBConfiguration {..} = ConnBuilder
  { cb_createConn = connect defaultConnectInfo
                      { connectHost     = dbHost
                      , connectPort     = dbPort
                      , connectUser     = dbUser
                      , connectPassword = dbPassword }
  , cb_destroyConn = close
  , cb_poolConfiguration = poolCfg }
  where
    poolCfg = PoolCfg
      { pc_stripes      = 5
      , pc_resPerStripe = 1000
      , pc_keepOpenTime = 180 }

main :: IO ()
main = do
  config :: Configuration <- maybe (error "wtf") pure
                             =<< fmap decode . BL.readFile
                             =<< fmap head getArgs
  -- putStrLn $ "Listening at " ++ show host ++ ":" ++ show port
  spockAsApp (spock (cfg config) app) >>= runSettings (settings config)
  where
    settings Configuration{..} = setPort (appPort confApp) . setHost (appHost confApp) $ defaultSettings
    sessCfg = defaultSessionCfg ()
    cfg config = SpockCfg
      { spc_initialState   = ()
      , spc_sessionCfg     = sessCfg
      , spc_maxRequestSize = Nothing
      , spc_database = PCConn $ connBuilder (confDB config)
      }
