import Hsmtlib.Solver
import Transformer2
import Hsmtlib.Parsers.ParseScript
import Text.Parsec.Prim              as Prim

pin ::(Show a) => [IO a] -> IO ()
pin [] = return ()
pin (x:xs) = do
    val <- x
    print val
    pin xs


main :: IO ()
main = do
    line <- getLine
    hs <- getLine
    file <- readFile line
    let ps = parse parseSource "" file 
    case ps of
        Left err -> print err
        Right src ->  writeFile hs (generateProgram "Z3" "Online" "AUFLIA" src)