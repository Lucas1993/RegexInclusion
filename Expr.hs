module Expr where

data Expr = Incl Reg Reg deriving Show
data Tipo = Tipo Char deriving Show
data Reg = Elem Tipo | Star Reg | Concat Reg Reg | Opt Reg Reg deriving Show
