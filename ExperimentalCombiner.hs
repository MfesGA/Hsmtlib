{-
  Experimental combiners to hide try and case, not working.
-}

(<#>) :: (IO (Either String ())) -> ((a -> IO b), a) -> IO( Either String  b)  
(<#>) either_io (f,arg)  = do
  either <- either_io
  case either of
    Left exception -> return $ Left exception
    Right x -> do
      resf <- tryIO f arg
      case resf of
        Left exception2 -> return $ Left $ show exception2
        Right value -> return $ Right value

(<&>):: IO(Either String a) -> (a -> IO b) -> IO (Either String b)
(<&>) either_io f  = do 
  either <- either_io
  case either of
    Left exception -> return $ Left exception
    Right x -> do
      resf <- tryIO f x
      case resf of
        Left exception2 -> return $ Left( show exception2)
        Right value -> return $ Right value
        
raiseTry ::(a -> IO b ) -> a -> IO(Either String b)
raiseTry f arg = do
  res <- tryIO f arg 
  case res of
    Left exception -> return $ Left (show exception)
    Right value -> return $ Right value
  
