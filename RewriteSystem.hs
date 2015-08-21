module RewriteSystem where

import Data.Function
import Data.List
import TermTypes
import Unfold

rewrite :: [Ineq] -> Bool
rewrite (x:xs)
	| IFalse `elem`  ans = False
	| isSaturated (ans ++ zs) = True
	| otherwise = rewrite $  (zs ++ ans)
     where ans  = applyRule z
           z:zs = clearRepeated $ x:xs

applyRule :: Ineq -> [Ineq]
applyRule ITrue = [ITrue]
applyRule IFalse = [IFalse]
applyRule (IneqO a b)  = [IneqO a b]
applyRule (Ineq a b)  
	| (is_reg1 a && (not $ is_reg1 b)) = [IFalse]
	| otherwise =  case (a,b) of
		(R0 _, _) -> unf a b ++ [IneqO a b]
		(R1 _, _) -> unf a b ++ [IneqO a b]

clearRepeated :: [Ineq] -> [Ineq]
clearRepeated ls = sort $ uniqueOld Nothing $ sortBy eqOld ls
	where 
		uniqueOld Nothing [] = []
		uniqueOld Nothing (a:as) = uniqueOld (Just a) as
		uniqueOld (Just a) [] = [a]
		uniqueOld (Just a) (x:xs) = if (eqOld a x) == EQ
			then uniqueOld (Just $ makeOld a) xs
			else a : (uniqueOld (Just x) xs)
		eqOld = compare `on` makeOld

		
		

--------- Auxiliary functions

is_reg1 :: Reg -> Bool
is_reg1 (R0 _) = False
is_reg1 (R1 _) = True

is_sreg0 :: [Reg] -> Bool
is_sreg0 = all (not . is_reg1)

makeOld :: Ineq -> Ineq
makeOld (Ineq a b) = IneqO a b
makeOld ineq = ineq

isSaturated :: [Ineq] -> Bool
isSaturated [] = True
isSaturated (x:xs) = case x of
	(IneqO _ _) -> isSaturated xs
	ITrue		-> isSaturated xs
	_ 			-> False
