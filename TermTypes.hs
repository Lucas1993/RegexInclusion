module TermTypes where

data Reg = R0 Reg0 | R1 Reg1 deriving (Show, Eq, Ord)

-- Regex w/o epsilon
data Reg0 = Empty | Elem Char | Concat0L Reg0 Reg | Concat0R Reg Reg0 | Opt0 Reg0 Reg0  deriving (Show, Eq, Ord)

-- Regex with epsilon
data Reg1 = Eps | Star Reg | Concat1 Reg1 Reg1 | Opt1L Reg1 Reg | Opt1R Reg Reg1  deriving (Show, Eq, Ord)

type SReg = [Reg] 

data Ineq = Ineq Reg Reg | IneqO Reg Reg | ITrue | IFalse deriving (Show, Eq, Ord)
type Conj = [Ineq]

-- Regular concatenation function

cnctReg :: Reg -> Reg -> Reg
cnctReg r1 r2 = case (r1, r2) of
	(R0 a, R0 b) -> R0 $ Concat0L a (R0 b)
	(R0 a, R1 b) -> R0 $ Concat0L a (R1 b)
	(R1 a, R0 b) -> R0 $ Concat0R (R1 a) b
	(R1 a, R1 b) -> R1 $ Concat1 a b

makeOpt :: Reg -> Reg -> Reg
makeOpt r1 r2 = case (r1, r2) of
                  (R0 Empty, _) -> r2
                  (_, R0 Empty) -> r1
                  (R0 a, R0 b) -> R0 $ Opt0 a b
                  (R0 a, R1 b) -> R1 $ Opt1R (R0 a) b
                  (R1 a, R0 b) -> R1 $ Opt1L a (R0 b)
                  (R1 a, R1 b) -> R1 $ Opt1L a (R1 b)

-- Builder functions

buildElem :: Char -> Reg
buildElem c = R0 $ Elem c

buildIneq :: Reg -> Reg -> Ineq
buildIneq r1 r2 = Ineq r1 r2

buildOpt :: Reg -> Reg -> Reg
buildOpt r1 r2 = case (r1, r2) of
	(R1 r1', _)      -> R1 $ Opt1L r1' r2
	(_, R1 r2')      -> R1 $ Opt1R r1 r2'
	(R0 r1', R0 r2') -> R0 $ Opt0 r1' r2'

buildConcat :: Reg -> Reg -> Reg
buildConcat r1 r2 = case (r1, r2) of
	(R0 r1', _)      -> R0 $ Concat0L r1' r2
	(_, R0 r2')      -> R0 $ Concat0R r1 r2'
	(R1 r1', R1 r2') -> R1 $ Concat1 r1' r2'

buildStar :: Reg -> Reg
buildStar r1 = R1 $ Star r1
