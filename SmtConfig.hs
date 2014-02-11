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
          
-- |this fuction will give back a config with the parms in the file specified in path
getconfig :: String -> IO( Either String Config)
getconfig path = do 
	    file <- B.readFile path
	    return $ eitherDecode file

-- | this function will give a new config when present a old config and a new path
chPath :: Config -> String -> Config
chPath c path_n=  Config {
					path= path_n,
					args= args c,
					default_mode=default_mode c,
					avaliable_modes= avaliable_modes c}
-- | this function will give a new config when present a old config and a new ARGS
chArgs :: Config -> [String] -> Config
chArgs c args_n=  Config {
					path= path c,
					args= args_n,
					default_mode=default_mode c,
					avaliable_modes= avaliable_modes c}
-- | this function will give a new config when present a old config and a new Default_mode
chDefaultMode :: Config -> String -> Config
chDefaultMode c default_mode_n=  Config {
					path= path c,
					args= args c,
					default_mode=default_mode_n,
					avaliable_modes= avaliable_modes c}
-- | this function will give a new config when present a old config and a new available modes
chAvailableModes :: Config -> [String] -> Config
chAvailableModes c available_modes_n=  Config {
					path= path c,
					args= args c,
					default_mode=default_mode c,
					avaliable_modes= available_modes_n}


-- | this function will create a config with the paramethers given
genconfig :: (String, [String], String, [String]) -> Config
genconfig (path_n, args_n, default_mode_n, avaliable_modes_n)= Config {
								path= path_n,
								args= args_n,
								default_mode=default_mode_n,
								avaliable_modes= avaliable_modes_n}

-- | this function will create and put a config in a file specified in path 
generateconfig :: (String, [String], String, [String]) -> String -> IO()
generateconfig config_parms file_name= do
								B.writeFile file_name $ encode $ genconfig config_parms
								 


