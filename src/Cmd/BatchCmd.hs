module Cmd.BatchCmd(executeBatch) where

import           Cmd.ProcCom.Process (Args, CmdPath, sendContext)
import           Cmd.Solver          (Result)
import           Data.List           (intercalate)
import           SMTLib2             (Command (CmdSetLogic), Name (N), Script,
                                      pp)
import           System.Process
import           Text.PrettyPrint    (render, Doc)


setLogic :: String -> Command
setLogic logic = CmdSetLogic (N logic)


executeBatch :: CmdPath -> Args -> String -> [Command]-> IO Result
executeBatch cmd args logic script =
    sendContext cmd args result
    where fScript = (setLogic logic) : script
          result = intercalate "\n" $ fmap (render.(pp :: Command -> Doc)) fScript
