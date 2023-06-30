import Data.Char (isDigit, isSpace)

data Token = NumToken Double | OperatorToken Char deriving (Show)

tokenize :: String -> Either String [Token]
tokenize "" = Right []
tokenize (x : xs)
  | isSpace x = tokenize xs
  | isDigit x || x == '.' =
      let (num, rest) = span (\c -> isDigit c || c == '.') (x : xs)
       in case reads num of
            [(n, "")] -> (NumToken n :) <$> tokenize rest
            _ -> Left $ "Número inválido: " ++ num
  | not (isOperator x) = Left $ "Caractere inválido: " ++ [x]
  | otherwise = (OperatorToken x :) <$> tokenize xs
  where
    isOperator c = c `elem` "+-*/()"

avalia :: String -> Either String Double
avalia expressao = do
  tokens <- tokenize expressao
  if null tokens
    then Left "Expressão vazia"
    else do
      postfix <- toPostfix tokens
      if null postfix
        then Left "Expressão vazia"
        else avaliaPostfix postfix []

toPostfix :: [Token] -> Either String [Token]
toPostfix tokens = reverse <$> exec tokens [] []
  where
    exec [] pilha saida = Right $ pilha ++ saida
    exec (NumToken n : ts) pilha saida = exec ts pilha (NumToken n : saida)
    exec (OperatorToken o : ts) pilha saida
      | o == '(' = exec ts (OperatorToken o : pilha) saida
      | o == ')' =
          case span (\t -> case t of OperatorToken '(' -> False; _ -> True) pilha of
            (leftPilha, []) -> Left "Parenteses errados"
            (leftPilha, rightPilha) -> exec ts (tail rightPilha) (reverse leftPilha ++ saida)
      | otherwise =
          let (maiorPrec, menorPrec) = span (\t -> case t of OperatorToken top -> precedence top >= precedence o; _ -> True) pilha
           in exec ts (OperatorToken o : menorPrec) (reverse maiorPrec ++ saida)

-- Função para avaliar a expressão posfixa
avaliaPostfix :: [Token] -> [Double] -> Either String Double
avaliaPostfix [] [result] = Right result
avaliaPostfix (NumToken n : ts) pilha = avaliaPostfix ts (n : pilha)
avaliaPostfix (OperatorToken o : ts) pilha = do
  nextPilha <- aplicaOperador o pilha
  avaliaPostfix ts nextPilha

aplicaOperador :: Char -> [Double] -> Either String [Double]
aplicaOperador o (b : a : pilha) = case o of
  '+' -> Right $ (a + b) : pilha
  '-' -> Right $ (a - b) : pilha
  '*' -> Right $ (a * b) : pilha
  '/' -> Right $ (a / b) : pilha
  _ -> Left $ "Operador inválido: " ++ [o]
aplicaOperador _ pilha = Right pilha

precedence :: Char -> Int
precedence '+' = 1
precedence '-' = 1
precedence '*' = 2
precedence '/' = 2
precedence _ = 0

main :: IO ()
main = do
  let expressao = "((((2.3 + 5.5) * 2)))"
  case avalia expressao of
    Right resultado -> putStrLn $ "Resultado: " ++ show resultado
    Left erro -> putStrLn $ "Erro: " ++ erro
