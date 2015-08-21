{
module AlexToken (Token(..),scanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

	$eol		;
	$white+		;
	$alpha		{ \s -> TokenChar (head s) }
	\(			{ \s -> TokenLParen }
	\)			{ \s -> TokenRParen }
	\<			{ \s -> TokenLess }
	\*			{ \s -> TokenStar }
	\+			{ \s -> TokenPlus }

{
data Token = TokenChar Char
             | TokenLParen
             | TokenRParen
             | TokenLess 
             | TokenStar 
             | TokenPlus 
			 deriving (Show)

scanTokens = alexScanTokens

}
