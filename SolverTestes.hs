import Process


-- z3 works fine.
z3 :: IO()
z3 = do
  smt <- beginProcess "z3" ["-smt2","-in"]
  send smt "(set-option :print-success true)\n" >>= print
  send smt "(declare-const a Int)\n" >>= print
  send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  send smt "(assert (> a 10))\n" >>= print
  send smt "(assert (< (f a true) 100))\n" >>= print
  send smt "(check-sat)\n" >>= print
  send smt "(get-model)\n" >>= print
  endProcess smt >>= print
  
 
-- sonolar inst working.
sonolar :: IO ()
sonolar = do
  smt <- beginProcess "sonolar" ["--print-model"]
  send smt "(set-option :print-success true)\n" >>= print
  send smt "(declare-const a Int)\n" >>= print
  send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  send smt "(assert (> a 10))\n" >>= print
  send smt "(assert (< (f a true) 100))\n" >>= print
  send smt "(check-sat)\n" >>= print
  send smt "(get-model)\n" >>= print
  endProcess smt >>= print
  

-- mathSat works fine.
mathSat :: IO()
mathSat = do
  smt <- beginProcess "mathsat" []
  send smt "(set-option :print-success true)\n" >>= print
  send smt "(declare-const a Int)\n" >>= print
  send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  send smt "(assert (> a 10))\n" >>= print
  send smt "(assert (< (f a true) 100))\n" >>= print
  send smt "(check-sat)\n" >>= print
  send smt "(get-model)\n" >>= print
  endProcess smt >>= print

stp :: IO()
stp = do
  smt <- beginProcess "stp" ["--SMTLIB2"]
  send smt "(set-option :print-success true)\n" >>= print
  send smt "(declare-const a Int)\n" >>= print
  send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  send smt "(assert (> a 10))\n" >>= print
  send smt "(assert (< (f a true) 100))\n" >>= print
  send smt "(check-sat)\n" >>= print
  send smt "(get-model)\n" >>= print
  endProcess smt >>= print




cvc42 :: IO ()
cvc42 = do
  solvre "cvc4" NOTHNG
  let script =  "(set-logic LRA)\n\
                 \(declare-fun lambda () Real)\n\
                 \(declare-fun lambdaprime () Real)\n\
                 \(declare-fun x5 () Real)\n"
  sendScript "cvc4" ["--smtlib-strict","--print-succes"] "temp.smt2" script >>= print

-- cvc dosen't work, breaks the pipe and prints to std out.   
cvc4 :: IO()
cvc4 = do
  smt <- beginProcess "cvc4" ["--smtlib-strict","--print-succes"]
  --CVC4 wont accept the next command and print the warning to std_err 
  send smt "(set-logic LRA)\n" >>= print 
  send smt "(declare-fun lambda () Real\n)" >>= print  
  send smt "(declare-fun lambdaprime () Real)\n" >>= print
  send smt "declare-fun x5 () Real)\n" >>= print
  --send smt "(check-sat)\n" >>= print
  --send smt "(get-model)\n" >>= print
  --send smt "(declare-const a Int)\n" >>= print 
  --send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  --send smt "(assert (> a 10))\n" >>= print
  --send smt "(assert (< (f a true) 100))\n" >>= print
  --send smt "(check-sat)\n" >>= print
  --send smt "(get-model)\n" >>= print
  endProcess smt  >>=print
