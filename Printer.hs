module Printer where

import LispTypes

printList :: [LispTypes] -> IO ()
printList (hd:[]) = printTypes hd
printList (hd:tl) = do printTypes hd
                       putStr " "
                       printList tl

printTypes :: LispTypes -> IO ()
printTypes (Symbol str) = putStr str
printTypes (List lst) = do putStr "("
                           printList lst
                           putStr ")"
printTypes (Pair head tail) = do putStr "("
                                 printTypes head
                                 putStr " . "
                                 printTypes tail
                                 putStr ")"
printTypes (Integer num) = putStr $ show num
printTypes (Float num) = putStr $ show num
printTypes (String str) = do putStr "\""
                             putStr str
                             putStr "\""
printTypes (Bool True) = putStr "#t"
printTypes (Bool False) = putStr "#f"
