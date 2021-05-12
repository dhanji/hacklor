-- Djikstra's shunting yard algorithm to parse infix notation expressions
--    3 + 4 - 1 => 3 4 1 - +
--    https://en.wikipedia.org/wiki/Shunting-yard_algorithm
import Data.List


precedence "("         = 1
precedence ")"         = 1
precedence "+"         = 2
precedence "-"         = 2
precedence "*"         = 3
precedence "/"         = 3
precedence "%"         = 3
precedence "^"         = 4


-- Pulls tokens apart into op stack and output, shunting on precedence rules
shunt ops output []    = (ops, output)
shunt ops output (e:es)
    | e == "("         = shunt (e : ops) output es
    | e == ")"         = shunt (ops \\ popBlock) (output ++ popBlock) es
    | isOperator       = shunt (e : (ops \\ popPrecedent)) (output ++ popPrecedent) es
    | otherwise        = shunt ops (output ++ [e]) es
    where
        popBlock       = takeWhile (/= "(") ops
        popPrecedent   = takeWhile (\x -> precedence x >= precedence e) ops
        isOperator     = elem e ["+", "-", "*", "/", "%", "^"]


-- Emits infix expressions as postfix (reverse polish) notation
parse expression       = let (ops, output) = shunt [] [] (words expression)
                         in output ++ (filter (\x -> notElem x ["(", ")"]) ops)


main                   = putStrLn $ show $ parse "3 + 4 * 2 / ( 1 - 5 )"