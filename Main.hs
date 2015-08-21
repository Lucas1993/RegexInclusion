import HappyParser
import AlexToken
import RewriteSystem
import TermTypes

loop :: IO ()
loop = do
	input <- getLine
	if input == "quit"
		then return () 
		else (print $ rewrite $ parse $ scanTokens input) >> loop


ineq = Ineq (R1 (Star (R0 (Elem 'a')))) ( R1 (Concat1 (Star (R0 (Elem 'a'))) (Star (R0 (Elem 'b')))) )

test :: IO ()
test = do
    res1 <- return $ applyRule ineq
    print res1

{-reg = Ineq (R0 $ Elem 'a') [R1 $ Star $ R0 $ Elem 'a']-}

main :: IO ()
main = loop
