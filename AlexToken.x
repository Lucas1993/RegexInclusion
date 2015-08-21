{
module AlexToken (Token(..),scanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alphaLow = [a-z]
$alphaUpper = [A-Z]
$eol   = [\n]

tokens :-

	$eol		;
	$white+		;
	$alphaLow	{ \s -> TokenConst (head s) }
	$alphaUpper	{ \s -> TokenVar (head s) }
	\(			{ \s -> TokenLParen }
	\)			{ \s -> TokenRParen }
	\<			{ \s -> TokenLess }
	\*			{ \s -> TokenStar }
	\+			{ \s -> TokenPlus }

{
data Token = TokenConst Char
             | TokenVar Char
             | TokenLParen
             | TokenRParen
             | TokenLess 
             | TokenStar 
             | TokenPlus 
			 deriving (Show)

scanTokens = alexScanTokens

}
