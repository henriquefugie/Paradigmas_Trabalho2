import Control.Exception
import Data.Char (isDigit, isSpace)

data Token = NumToken Double | OperatorToken Char deriving (Show)

data ParsingError = InvalidNumber | UnbalancedParentheses | InvalidExpression deriving (Show)

instance Exception ParsingError

tokenize :: String -> [Token]
tokenize "" = []
tokenize (x : xs)
  | isSpace x = tokenize xs
  | isDigit x =
      let (num, rest) = span isDigit (x : xs)
       in NumToken (read num) : tokenize rest
  | otherwise = OperatorToken x : tokenize xs

avalia :: String -> Either ParsingError Double
avalia expressao = do
  valid <- checkValidity expressao
  if valid
    then Right (head pilha)
    else Left InvalidExpression
  where
    tokens = tokenize expressao
    postfix = toPostfix tokens
    pilha = avaliaPostfix postfix []

toPostfix :: [Token] -> [Token]
toPostfix tokens = reverse $ exec tokens [] []
  where
    exec [] pilha saida = pilha ++ saida
    exec (NumToken n : ts) pilha saida = exec ts pilha (NumToken n : saida)
    exec (OperatorToken o : ts) pilha saida
      | o == '(' = exec ts (OperatorToken o : pilha) saida
      | o == ')' =
          let (leftPilha, rightPilha) = span (\t -> case t of OperatorToken '(' -> False; _ -> True) pilha
           in exec ts (tail rightPilha) (reverse leftPilha ++ saida)
      | otherwise =
          let (maiorPrec, menorPrec) = span (\t -> case t of OperatorToken top -> precedence top >= precedence o; _ -> True) pilha
           in exec ts (OperatorToken o : menorPrec) (reverse maiorPrec ++ saida)

-- Função para avaliar a expressão posfixa
avaliaPostfix :: [Token] -> [Double] -> [Double]
avaliaPostfix [] pilha = pilha
avaliaPostfix (NumToken n : ts) pilha = avaliaPostfix ts (n : pilha)
avaliaPostfix (OperatorToken o : ts) pilha = avaliaPostfix ts (aplicaOperador o pilha)

aplicaOperador :: Char -> [Double] -> [Double]
aplicaOperador o (b : a : pilha) = case o of
  '+' -> (a + b) : pilha
  '-' -> (a - b) : pilha
  '*' -> (a * b) : pilha
  '/' -> (a / b) : pilha
  _ -> pilha
aplicaOperador _ pilha = pilha

precedence :: Char -> Int
precedence '+' = 1
precedence '-' = 1
precedence '*' = 2
precedence '/' = 2
precedence _ = 0

checkValidity :: String -> Either ParsingError Bool
checkValidity expressao = do
  let tokens = tokenize expressao
  let parentheses =
        filter
          ( \t -> case t of
              OperatorToken '(' -> True
              OperatorToken ')' -> True
              _ -> False
          )
          tokens
  if length parentheses `mod` 2 == 0
    then Right True
    else Left UnbalancedParentheses

main :: IO ()
main = do
  result <- try (evaluate (avalia "2.3+3.5")) :: IO (Either SomeException (Either ParsingError Double))
  case result of
    Right (Right resultado) -> putStrLn $ "Resultado: " ++ show resultado
    Right (Left err) -> putStrLn $ "Erro: " ++ show err
    Left ex -> putStrLn $ "Erro: " ++ show ex
