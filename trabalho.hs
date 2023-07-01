-- isDigit verifica se eh digito e isSpace verifica se eh espaco
import Data.Char (isDigit, isSpace)

-- Definição do tipo de dado Token, que representa um token da expressão
data Token = NumToken Double | OperatorToken Char deriving (Show)

-- Recebe um operador e retorna a precedencia do mesmo
precedencia :: Char -> Int
precedencia '+' = 1
precedencia '-' = 1
precedencia '*' = 2
precedencia '/' = 2
precedencia _ = 0

-- Função que aplica um operador a uma pilha de números
aplicaOperador :: Char -> [Double] -> Either String [Double]
aplicaOperador operador (b : a : pilha) = case operador of
  '*' -> Right ((a * b) : pilha)
  '/' -> Right ((a / b) : pilha)
  '+' -> Right ((a + b) : pilha)
  '-' -> Right ((a - b) : pilha)
  _ -> Left ("Operador inválido: " ++ [operador])
aplicaOperador _ pilha = Right pilha

-- Função que converte uma expressão em uma lista de tokens
tokenize :: String -> Either String [Token]
tokenize "" = Right [] -- Se a string estiver vazia, retorna uma lista vazia
tokenize (x : xs)
  | isSpace x = tokenize xs -- Se for um espaço em branco, ignora e continua a tokenização
  | isDigit x || x == '.' = -- Se for um dígito ou um ponto decimal, trata como número
      -- num recebe a parte inteira, e rest recebe a parte decimal de um numero
      let (num, rest) = span (\c -> isDigit c || c == '.') (x : xs) -- O span eh reponsavel por separar a expressao em uma sequencia inicial de digitos e o que vem depois do ponto, ou seja, a parte decimal
       in case reads num of -- Tenta converter a sequência em um número
            [(n, "")] -> do -- Se for um número válido, adiciona à lista de tokens e continua a tokenização
              tokens <- tokenize rest
              return (NumToken n : tokens)
            _ -> Left ("Número inválido: " ++ num) -- Se não for um número válido, retorna um erro
  | not (isOperator x) = Left ("Caractere inválido: " ++ [x]) -- Se for um caractere inválido, retorna um erro
  | otherwise = do -- Trata como operador e continua a tokenização
      tokens <- tokenize xs
      return (OperatorToken x : tokens)
  where
    isOperator c = c `elem` "+-*/()" -- Verifica se o caracter esta presente em "+-*/()"

-- Função que converte uma lista de tokens para notação posfixa
posFixado :: [Token] -> Either String [Token]
posFixado tokens = do
  let exec [] pilha saida = Right (pilha ++ saida) -- Se não houver mais tokens, retorna a pilha e a saída concatenadas em ordem reversa
      exec (NumToken n : ts) pilha saida = exec ts pilha (NumToken n : saida) -- Se for um número, adiciona à saída
      exec (OperatorToken operador : ts) pilha saida
        | operador == '(' = exec ts (OperatorToken operador : pilha) saida -- Se for um parêntese de abertura, adiciona à pilha
        | operador == ')' = case span (\t -> case t of OperatorToken '(' -> False; _ -> True) pilha of -- Se for um parêntese de fechamento
                        (_, []) -> Left "Parenteses errados" -- Se não houver parênteses de abertura correspondentes na pilha, retorna um erro
                        (esquerdaPilha, direitaPilha) -> exec ts (tail direitaPilha) (reverse esquerdaPilha ++ saida) -- Remove os parênteses de abertura correspondentes da pilha e adiciona à saída
        | otherwise =
            let (maiorPrec, menorPrec) = span (\t -> case t of OperatorToken top -> precedencia top >= precedencia operador; _ -> True) pilha -- Separa os operadores com precedência maior ou igual e menor
             in exec ts (OperatorToken operador : menorPrec) (reverse maiorPrec ++ saida) -- Adiciona à pilha os operadores com precedência menor e adiciona à saída os operadores com precedência maior
  fmap reverse (exec tokens [] [])

-- Função para avaliar a expressão posfixa
avaliaPosFixado :: [Token] -> [Double] -> Either String Double
avaliaPosFixado [] [result] = Right result -- Se não houver mais tokens e restar um único número na pilha, retorna esse número como resultado
avaliaPosFixado (NumToken n : ts) pilha = avaliaPosFixado ts (n : pilha) -- Se for um número, adiciona à pilha
avaliaPosFixado (OperatorToken operador : ts) pilha = do
  nextPilha <- aplicaOperador operador pilha -- Aplica o operador à pilha
  avaliaPosFixado ts nextPilha -- Avalia o restante da expressão
avaliaPosFixado _ _ = Left "Expressão inválida: Padrão de correspondência não encontrado." -- Se ocorrer um padrão de correspondência inválido, retorna um erro

-- A partir do either a funcao verifica se a avaliacao da expressao deu certo ou errado, se houver um retorno do either do tipo Right a funcao esta valida, se for Left, apresenta um erro
avalia :: String -> Either String Double
avalia expressao = do
  tokens <- tokenize expressao -- Faz a tokenização da expressão
  if null tokens
    then Left "Expressão vazia" -- Se não houver tokens, retorna um erro em Left de expressão vazia
    else do -- Se houver algo nos tokens
      expressaoPosFixa <- posFixado tokens -- Converte os tokens para notação posfixa
      if null expressaoPosFixa
        then Left "Expressão vazia" -- Se não houver tokens na notação posfixa, retorna um erro de expressão vazia
        else avaliaPosFixado expressaoPosFixa [] -- Avalia a expressão posfixa

main :: IO ()
main = do
  let expressao = "((10 + 3) * (8 - 4)) / (7 + 5)"
  case avalia expressao of
    Right resultado -> putStrLn ("Resultado: " ++ show resultado)
    Left erro -> putStrLn ("Erro: " ++ erro)
