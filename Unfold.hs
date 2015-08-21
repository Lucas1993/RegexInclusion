module Unfold where

import TermTypes
import Deriv

unf :: Reg -> Reg -> Conj 
unf r s = case r of
	R0 Empty -> [ITrue]
	R1 Eps -> [ITrue]
	R0 (Elem x) -> [Ineq (R1 Eps) (der x s)]
	R1 (Star a) -> unf2 a r s
	R0 (Opt0 r1 r2)	-> unf (R0 r1) s ++ unf (R0 r2) s
	R1 (Opt1L r1 r2)	-> unf (R1 r1) s ++ unf r2 s
	R1 (Opt1R r1 r2)	-> unf r1 s ++ unf (R1 r2) s
	R0 (Concat0L r1 r2) -> unf2 (R0 r1) r2 s
	R0 (Concat0R r1 r2) -> case r1 of
					     	R0 _ -> unf2 r1 (R0 r2) s
					     	R1 _ -> (unf2 r1 (R0 r2) s) ++ (unf (R0 r2) s)
	R1 (Concat1 r1 r2)  -> (unf2 (R1 r1) (R1 r2) s) ++ (unf (R1 r2) s)

unf2 :: Reg -> Reg -> Reg -> [Ineq]
unf2 r c s = case r of
	R0 Empty -> [ITrue]
	R1 Eps -> [ITrue]
	R0 (Elem x) -> [Ineq c (der x s)]
	R1 (Star a) -> unf2 a (cnctReg r c) s
	R0 (Opt0 r1 r2)	-> unf2 (R0 r1) c s ++ unf2 (R0 r2) c s
	R1 (Opt1L r1 r2)	-> unf2 (R1 r1) c s ++ unf2 r2 c s
	R1 (Opt1R r1 r2)	-> unf2 r1 c s ++ unf2 (R1 r2) c s
	R0 (Concat0L r1 r2) -> unf2 (R0 r1) (cnctReg r2 c) s
	R0 (Concat0R r1 r2) -> case r1 of
					     	R0 _ -> unf2 r1 (cnctReg (R0 r2) c) s
					     	R1 _ -> (unf2 r1 (cnctReg (R0 r2) c) s) ++ (unf2 (R0 r2) c s)
	R1 (Concat1 r1 r2)  -> (unf2 (R1 r1) (cnctReg (R1 r2) c) s) ++ (unf2 (R1 r2) c s)

