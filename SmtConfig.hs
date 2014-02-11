{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B

data Config = Config
            { path :: String
            , args :: [String]
            , default_mode :: String
            , avaliable_modes :: [String]
            }deriving(Show)




instance FromJSON Config where
  parseJSON (Object v) = 
                        Config <$> v .: "path"
                               <*> v .: "args"
                               <*> v .: "default_mode" 
                               <*> v .: "avaliable_modes"
  parseJSON _          = mzero

instance ToJSON Config where
 toJSON (Config path args default_mode avaliable_modes)=
    object[ "path" .= path
          , "args" .= args
          ,  "default_mode" .= default_mode
          ,  "avaliable_modes" .= avaliable_modes
          ]
          


main :: IO ()
main  = do
  file <- B.readFile "solvers_config/cvc4.json" :: IO B.ByteString
  case (decode file) of
    Nothing -> print "Nada"
    Just x -> print (x::Config)
  
