module Substitution (updateSubs, Substitution, updateReg) where

import TermTypes
import Data.Map.Strict as Map

type Substitution = Map Char Reg

updateSubs :: Map Char Reg -> (Char, Reg) -> Map Char Reg
updateSubs subs (k, v) = Map.insert k v upMap
    where
        upMap = updateMapElems subs (k, v)

updateMapElems :: Map k Reg -> (Char, Reg) -> Map k Reg
updateMapElems s (k, v) = Map.map (updateReg (k, v)) s

updateReg :: (Char, Reg) -> Reg -> Reg
updateReg (var, term) target = case target of
    R0 _ -> updateR0 (var, term) target
    R1 _ -> updateR1 (var, term) target

updateR0 :: (Char, Reg) -> Reg -> Reg
updateR0 (var, term) target = case getR0 target of
    Elem a -> R0 $ Elem a
    Concat0L r1 r2 -> cnctReg (updateR0 (var, term) $ R0 r1) (updateReg (var, term) r2)
    Concat0R r1 r2 -> cnctReg (updateReg (var, term) r1) (updateR0 (var, term) $ R0 r2)
    Opt0     r1 r2 -> makeOpt (updateR0 (var, term) $ R0 r1) (updateR0 (var, term) $ R0 r2)
    Var      x     -> if x == var
                        then term
                        else R0 $ Var x

updateR1 :: (Char, Reg) -> Reg -> Reg
updateR1 (var, term) target = case getR1 target of
    Eps -> R1 $ Eps
    Concat1 r1 r2 -> cnctReg (updateR1 (var, term) $ R1 r1) (updateR1 (var, term) $ R1 r2)
    Opt1L    r1 r2 -> makeOpt (updateR1 (var, term) $ R1 r1) (updateReg (var, term) r2)
    Opt1R    r1 r2 -> makeOpt (updateReg (var, term) r1) (updateR1 (var, term) $ R1 r2)
    Star     r1    -> R1 $ Star (updateReg (var, term) r1) 

getR0 :: Reg -> Reg0
getR0 (R0 r) = r
getR0 _ = error "Not R0!"

getR1 :: Reg -> Reg1
getR1 (R1 r) = r
getR1 _ = error "Not R1!"
