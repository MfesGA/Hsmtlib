module  Hsmtlib.Solvers.Cmd.BatchCmd(executeBatch) where

import           Data.List                           (intercalate)
import           Hsmtlib.Solvers.Cmd.ProcCom.Process (Args, CmdPath,
                                                      sendContext)
import           Smtlib                             (Command (CmdSetLogic),
                                                      Name (N), pp)
import           Text.PrettyPrint                    (Doc, render)


setLogic :: String -> Command
setLogic logic = CmdSetLogic (N logic)


executeBatch :: CmdPath -> Args -> String -> [Command]-> IO String
executeBatch cmd args logic script =
    sendContext cmd args result
    where fScript = setLogic logic : script
          result = intercalate "\n" $ fmap (render.(pp :: Command -> Doc)) fScript
