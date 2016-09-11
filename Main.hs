import Parser
import Printer
import Eval
import System.Environment

main :: IO ()
main = do args <- getArgs
          case (readExpr (args !! 0)) of
            Left err -> putStr $ "No match: " ++ show err
            Right val -> do Printer.printTypes (let (_,val2) = eval ([],val) in
                                                  val2)
          putStrLn ""
