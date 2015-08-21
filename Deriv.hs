module Deriv where

import TermTypes

der :: Char -> Reg -> Reg
der x r = case r of
	R0 Empty            -> R0 Empty
	R1 Eps              -> R0 Empty
	R0 (Elem y)         -> if x == y
					     	then R1 Eps
					     	else R0 Empty
	R0 (Opt0 r1 r2)     -> makeOpt (der x ( R0 r1 )) (der x ( R0 r2 ))
	R1 (Opt1L r1 r2)    -> makeOpt (der x ( R1 r1 )) (der x r2)
	R1 (Opt1R r1 r2)    -> makeOpt (der x r1) (der x ( R1 r2 ))
	R1 (Star r1)        -> der2 x r1 r
	R0 (Concat0L r1 r2) -> der2 x (R0 r1) r2
	R0 (Concat0R r1 r2) -> case r1 of
					     	R0 _ -> der2 x r1 (R0 r2)
					     	R1 _ -> makeOpt (der2 x r1 (R0 r2)) (der x ( R0 r2 ))
	R1 (Concat1 r1 r2)  -> makeOpt (der2 x (R1 r1) (R1 r2)) (der x ( R1 r2 ))

der2 :: Char -> Reg -> Reg -> Reg
der2 x y c = case y of
	R0 Empty            -> ( R0 Empty )
	R1 Eps              -> ( R0 Empty )
	R0 (Elem z)         -> if x == z
							then c
							else ( R0 Empty )
	R0 (Opt0 r1 r2)     -> makeOpt (der2 x ( R0 r1 ) c) (der2 x ( R0 r2 ) c)
	R1 (Opt1L r1 r2)    -> makeOpt (der2 x ( R1 r1 ) c) (der2 x ( r2 ) c)
	R1 (Opt1R r1 r2)    -> makeOpt (der2 x ( r1 ) c) (der2 x ( R1 r2 ) c)
	R1 (Star r)			-> der2 x r (cnctReg y c)
	R0 (Concat0L r1 r2) -> der2 x (R0 r1) (cnctReg r2 c)
	R0 (Concat0R r1 r2) -> case r1 of
					     	R0 _ -> der2 x r1 $ cnctReg (R0 r2) c
					     	R1 _ -> makeOpt (der2 x r1 $ cnctReg (R0 r2) c) (der2 x (R0 r2) c)
	R1 (Concat1 r1 r2)  -> makeOpt (der2 x (R1 r1) $ cnctReg (R1 r2) c) (der2 x (R1 r2) c)
