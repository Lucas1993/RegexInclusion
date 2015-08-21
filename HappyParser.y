{
module HappyParser where

import AlexToken
import TermTypes
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	TIPO		{ TokenChar $$ }
	'(' 		{ TokenLParen }
	')' 		{ TokenRParen }
	'<' 		{ TokenLess } 
	'*' 		{ TokenStar } 
	'+' 		{ TokenPlus } 
%%

Expr : Term '<' Term { [buildIneq $1 $3] }

Term : Term '+' Term { buildOpt $1 $3 }
	 | Term1		 { $1 }

Term1 : Term Term { buildConcat $1 $2 }
	  | Term2	  { $1 }

Term2 : '(' Term ')' { $2 }
	  | Term '*' { buildStar $1 }
      | TIPO	 { buildElem $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

}
