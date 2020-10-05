data Cmd = Cursor Int| Backspace Int| Delete Int| Insert String deriving (Read)


insertText :: String -> String-> Int -> String
insertText str txtList cursor = 
                            let inicio = take (cursor) (str)
                                fim = drop (cursor) (str)
                            in inicio ++ txtList ++ fim

deleteText :: String -> Int -> Int -> String
deleteText str cursor n = 
                            let inicio = take (cursor) (str)
                                fim = drop (cursor+n) (str)
                            in inicio ++ fim

backspaceText :: String -> Int -> Int -> String
backspaceText str cursor n = 
                    let inicio = take (max 0 (cursor-n)) (str)
                        fim = drop (cursor) (str)
                    in inicio ++  fim



editTextAux :: String -> [Cmd]-> Int -> String
editTextAux str [] cursor = str
editTextAux str (Insert txtList:as) cursor = editTextAux (insertText str txtList cursor) (as) (cursor)
editTextAux str (Delete n:as) cursor = editTextAux (deleteText str cursor n) (as) (cursor)
editTextAux str (Backspace n:as) cursor = editTextAux (backspaceText str cursor n) (as) (cursor-n)
editTextAux str (Cursor n:as) cursor = editTextAux (str) (as) (cursor + n )

editText :: String -> [Cmd] -> String
editText str [] = str
editText str cmd = editTextAux str cmd 0
