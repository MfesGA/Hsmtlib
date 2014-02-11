import Process.hs

data Mode = Default | Online | Context | Script deriving Show

data Smt = SMT
        { name :: String
        , arguments :: [String]
        , mode :: Mode
        , context :: [String]
        , process :: Maybe Process
        }

 
solver :: String -> IO Smt
solver name = do
   config <- smtConfig String
  
