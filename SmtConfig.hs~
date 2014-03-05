{-# LANGUAGE OverloadedStrings #-}

module SmtConfig where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B

data  Mode = Online | Script | Context

data Config = Config
            { path :: String
            , args :: [String]
            , defaultMode :: String
            , avaliableModes :: [String]
            }deriving(Show)

instance FromJSON Config where
  parseJSON (Object v) = 
                        Config <$> v .: "path"
                               <*> v .: "args"
                               <*> v .: "defaultMode" 
                               <*> v .: "avaliableModes"
  parseJSON _          = mzero

instance ToJSON Config where
 toJSON (Config path args defaultMode avaliableModes) =
    object[ "path" .= path
               , "args" .= args
               , "defaultMode" .= defaultMode
              , "avaliableModes" .= avaliableModes
              ]
          
-- |this fuction will give back a config with the parms in the file specified in path
getconfig :: String -> IO( Either String Config)
getconfig path = do 
	    file <- B.readFile path
	    return $ eitherDecode file

-- | this function will give a new config when present a old config and a new path
chPath :: Config -> String -> Config
chPath c path_n=  Config {
					path = path_n,
					args = args c,
					defaultMode=defaultMode c,
					avaliableModes= avaliableModes c}
-- | this function will give a new config when present a old config and a new ARGS
chArgs :: Config -> [String] -> Config
chArgs c args_n=  Config {
					path= path c,
					args= args_n,
					defaultMode=defaultMode c,
					avaliableModes= avaliableModes c}
-- | this function will give a new config when present a old config and a new defaultMode
chDefaultMode :: Config -> String -> Config
chDefaultMode c defaultMode_n=  Config {
					path= path c,
					args= args c,
					defaultMode=defaultMode_n,
					avaliableModes= avaliableModes c}
-- | this function will give a new config when present a old config and a new available modes
chAvailableModes :: Config -> [String] -> Config
chAvailableModes c available_modes_n=  Config {
					path= path c,
					args= args c,
					defaultMode=defaultMode c,
					avaliableModes= available_modes_n}


-- | this function will create a config with the paramethers given
genconfig :: (String, [String], String, [String]) -> Config
genconfig (path_n, args_n, defaultMode_n, avaliableModes_n)= Config {
								path= path_n,
								args= args_n,
								defaultMode=defaultMode_n,
								avaliableModes= avaliableModes_n}

-- | this function will create and put a config in a file specified in path 
generateconfig :: (String, [String], String, [String]) -> String -> IO()
generateconfig config_parms file_name= B.writeFile file_name $ encode $ genconfig config_parms
								 


