import Parser
import Printer
import System.Environment

main :: IO ()
main = do args <- getArgs
          case (readExpr (args !! 0)) of
            Left err -> putStr $ "No match: " ++ show err
            Right val -> printTypes val
          putStrLn ""
