module  Hsmtlib.Solvers.Cmd.Parser.CmdResult where

import           Hsmtlib.Solver                     as Solv
import           Hsmtlib.Solvers.Cmd.Parser.Parsers
import           Hsmtlib.Solvers.Cmd.Parser.Syntax  as S
import           Text.ParserCombinators.Parsec.Prim (parse)

genResponse :: String -> Result
genResponse stg =
    case result of
        Left err -> Solv.UError $ show err
        Right cmdRep -> case cmdRep of
                            CmdGenResponse S.Success -> Solv.Success
                            CmdGenResponse S.Unsupported -> Solv.Unsupported
                            CmdGenResponse (S.Error err) -> Solv.Error err
    where result = parse parseCmdGenResponse "" stg


checkSatResponse :: String -> Result
checkSatResponse stg =
    case result of
        Left err -> Solv.UError $ show err
        Right cmdRep -> case cmdRep of
                            S.Sat -> Solv.Sat
                            S.Unsat -> Solv.Unsat
                            S.Unknown -> Solv.Unknown
    where result = parse parseCheckSatResponse "" stg

