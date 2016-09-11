module Printer where

import LispTypes

printList :: [LispTypes] -> IO ()
printList (hd:[]) = printTypes hd
printList (hd:tl) = do printTypes hd
                       putStr " "
                       printList tl

printTypes :: LispTypes -> IO ()
printTypes (LispSymbol str) = do putStr "'"
                                 putStr str

printTypes (LispList lst) = do putStr "("
                               printList lst
                               putStr ")"
printTypes (LispPair head tail) = do putStr "("
                                     printTypes head
                                     putStr " . "
                                     printTypes tail
                                     putStr ")"
printTypes (LispInteger num) = putStr $ show num
printTypes (LispFloat num) = putStr $ show num
printTypes (LispString str) = do putStr "\""
                                 putStr str
                                 putStr "\""
printTypes (LispTrue) = putStr "#t"
printTypes (LispFalse) = putStr "#f"
