import Data.Char (isDigit, isSpace)

data Token = NumToken Double | OperatorToken Char deriving (Show)

precedencia :: Char -> Int
precedencia '+' = 1
precedencia '-' = 1
precedencia '*' = 2
precedencia '/' = 2
precedencia _ = 0

aplicaOperador :: Char -> [Double] -> Either String [Double]
aplicaOperador operador (b : a : pilha) = case operador of
  '*' -> Right ((a * b) : pilha)
  '/' -> Right ((a / b) : pilha)
  '+' -> Right ((a + b) : pilha)
  '-' -> Right ((a - b) : pilha)
  _ -> Left ("Operador inválido: " ++ [operador])
aplicaOperador _ pilha = Right pilha

tokenize :: String -> Either String [Token]
tokenize expr = tokenizeWithParens ('(' : expr ++ ")") -- Adiciona parênteses ao redor da expressão

tokenizeWithParens :: String -> Either String [Token]
tokenizeWithParens "" = Right []
tokenizeWithParens (x : xs)
  | isSpace x = tokenizeWithParens xs
  | isDigit x || x == '.' =
    let (num, rest) = span (\c -> isDigit c || c == '.') (x : xs)
     in case reads num of
          [(n, "")] -> do
            tokens <- tokenizeWithParens rest
            return (NumToken n : tokens)
          _ -> Left ("Número inválido: " ++ num)
  | not (isOperator x) = Left ("Caractere inválido: " ++ [x])
  | otherwise = do
      tokens <- tokenizeWithParens xs
      return (OperatorToken x : tokens)
  where
    isOperator c = c `elem` "+-*/()"

posFixado :: [Token] -> Either String [Token]
posFixado tokens = do
  let exec [] pilha saida = Right (pilha ++ saida)
      exec (NumToken n : ts) pilha saida = exec ts pilha (NumToken n : saida)
      exec (OperatorToken operador : ts) pilha saida
        | operador == '(' = exec ts (OperatorToken operador : pilha) saida
        | operador == ')' = case span (\t -> case t of OperatorToken '(' -> False; _ -> True) pilha of
                        (_, []) -> Left "Parenteses errados"
                        (esquerdaPilha, direitaPilha) -> exec ts (tail direitaPilha) (reverse esquerdaPilha ++ saida)
        | otherwise =
            let (maiorPrec, menorPrec) = span (\t -> case t of OperatorToken top -> precedencia top >= precedencia operador; _ -> True) pilha
             in exec ts (OperatorToken operador : menorPrec) (reverse maiorPrec ++ saida)
  fmap reverse (exec tokens [] [])

avaliaPosFixado :: [Token] -> [Double] -> Either String Double
avaliaPosFixado [] [result] = Right result
avaliaPosFixado (NumToken n : ts) pilha = avaliaPosFixado ts (n : pilha)
avaliaPosFixado (OperatorToken operador : ts) pilha = do
  nextPilha <- aplicaOperador operador pilha
  avaliaPosFixado ts nextPilha
avaliaPosFixado _ _ = Left "Expressão inválida: Padrão de correspondência não encontrado."

avalia :: String -> Either String Double
avalia expressao = do
  tokens <- tokenize expressao
  if null tokens
    then Left "Expressão vazia"
    else do
      expressaoPosFixa <- posFixado tokens
      if null expressaoPosFixa
        then Left "Expressão vazia"
        else avaliaPosFixado expressaoPosFixa []

main :: IO ()
main = do
  putStrLn "Digite uma expressão:"
  expressao <- getLine
  case avalia expressao of
    Right resultado -> putStrLn ("Resultado: " ++ show resultado)
    Left erro -> putStrLn ("Erro: " ++ erro)
