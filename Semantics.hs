module Semantics where 


--devolve o symbol
--ValidateSimbol a = ...
type Symbol = String
type Expression= String
type Numeral = String


set_logic :: Symbol -> String 
set_logic a = "( set-logic"++ a ++ ")\n"

-- TODO handeling with sorts...
declare_fun:: Symbol -> String
declare_fun a = "( declare-fun " ++  a ++")\n"  

declare_const:: Symbol -> String
declare_const a = "( declare-const " ++  a ++")\n"  


declare_sort_aux:: Symbol-> Numeral -> String
declare_sort_aux a b = "(declare-sort " ++  a ++  b ++ ")\n" 


define_sort:: [Symbol] -> Expression-> String 
define_sort a b = "(define-sort " ++ expand(a) ++  b ++ ")\n"  

expand :: [String] -> String
expand (a:b) =  a ++ " " ++ expand(b)


assert:: Expression -> String 
assert a = "(assert " ++  a ++ ")\n" 

get_assertions :: String
get_assertions = "(get-assertions)\n" 

check_sat :: String
check_sat = "(check-sat)\n"

get_proof :: String 
get_proof = "(get-proof)\n"

get_unsat_core :: String 
get_unsat_core = "(get-unsat-core)\n"


--get_value :: [Expression] -> String 

--get_assignement
