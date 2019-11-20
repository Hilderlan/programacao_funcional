-- Questão 1 Max
module ArvBusBin(ArvBusBin) where
  data ArvBusBin t = Nil | No (ArvBusBin t) t (ArvBusBin t) deriving (Eq, Ord, Show)

  -- Letra a (Calcular a profundidade de uma árvore de busca binária do tipo t)
  profArvBusBin :: ArvBusBin t -> Int -- maior distância de uma folha à raiz
  profArvBusBin (Nil) = 0
  -- profArvBusBin (No Nil n Nil) = 0
  profArvBusBin (No xt _ yt) = 1 + max (profArvBusBin xt) (profArvBusBin yt)

  -- Letra b (Construir umas ABB de t a partir de uma lista de um tipo t)
  fazABB :: (Ord t) => [t] -> ArvBusBin t
  fazABB [ ] = Nil
  fazABB (x : xs) = No (fazABB ys) x (fazABB zs)
    where (ys, zs) = particao (<= x) xs

  particao :: (t -> Bool) -> [t] -> ([t], [t])
  particao p xs = (filter p xs, filter (not . p) xs)

  -- Letra c (Transformar uma ABB de um tipo t numa lista ordenada [t])
  arvBBtoLista :: (Ord t) => ArvBusBin t -> [t]
  arvBBtoLista Nil = []
  arvBBtoLista (No xt n yt) = arvBBtoLista xt ++ [n] ++ arvBBtoLista yt

  -- Letra d (Verificar se um determinado valor do tipo t pertence ou não á uma arbore ArvBusBin t)
  buscarElemento :: (Ord t) => t -> ArvBusBin t -> Bool
  buscarElemento _ (Nil) = False
  buscarElemento elem (No xt n yt)
    | elem == n = True
    | elem < n = buscarElemento elem xt
    | otherwise = buscarElemento elem yt

-- Questão 1 Goku

  -- Letra a (Inserir um valor do tipo t nesta árvore)
  𝑑𝑎𝑡𝑎 (𝑂𝑟𝑑 𝑡) => 𝐴𝑟𝑣𝐵𝐵 𝑡 = 𝑁𝑖𝑙 | 𝑁𝑜 (𝐴𝑟𝑣𝐵𝐵 𝑡)𝑡 (𝐴𝑟𝑣𝐵𝐵 𝑡) deriving (Eq, Ord, Show)

  insere :: (Ord t) => t -> ArvBusBin t -> ArvBusBin t
  insere x Nil = No Nil x Nil
  insere x (No xt n yt)
  |x < n = No (insere x xt) n yt
  |x == n = No xt n yt
  |x > n = No xt n (insere x yt)

  -- Letra b (Remover um valor do tipo t nesta árvore)
  remove :: (Ord t) => t -> ArvBusbin t -> ArvBusBin t
  remove x Nil = Nil
  remove x (No xt n yt)
  |x < n = No (remove x xt) n yt
  |x > n = No xt n (remove x yt)
  |x == n = junta xt yt

  junta :: (Ord t) => ArvBusBin t->ArvBusBin t->ArvBusBin t
  junta Nil ad = ad
  junta ae Nil = ae
  junta (No aee x aed) (No ade y add)
  = No (No aee x aed) k (remove k (No ade y add))
  where k = minArv (No ade y add)

  minArv :: (Ord t) => ArvBusBin t -> t
  minArv (No Nil x _) = x
  minArv (No xt _ _) = minArv xt

-- Questão 2

  -- Essa abaixo é O(n*n)
  reverse :: [t] -> [t]
  reverse [ ] = [ ]
  reverse (x : xs) = reverse xs ++ [x]

  -- Essa é a reposta, em ordem O(n)
  revaux :: [t] -> [t] -> [t]
  revaux (a:la) ln = revaux la (a:ln)
  revaux [ ] ln = ln

  reverse :: [t] -> [t]
  reverse l = revaux l [ ]

-- Questão 3

    -- Fila primeira implementação
  module Queue (Queue, enqueue, dequeue, front, queueEmpty, newQueue) where
  enqueue :: t -> Queue t -> Queue t
  dequeue :: Queue t -> Queue t
  front :: Queue t -> t
  queueEmpty :: Queue t -> Bool
  newQueue :: Queue t
  dataQueue t = Fila [t]

  enqueue x (Fila q) = Fila (q ++ [x]) -- insere no final da lista
  dequeue (Fila (x : xs)) = Fila xs -- retira o elemento da cabeca da lista
  dequeue _ = error "Fila de espera vazia“
  front (Fila (x : _)) = x -- o front eh o elemento a ser retirado
  front _ = error "Fila de espera vazia“
  queueEmpty (Fila [ ]) = True
  queueEmpty _ = False
  newQueue = (Fila [ ])
  instance (Show t) => Show (Queue t) where
  show (Fila [ ]) = ">"
  show (Fila (x : xs)) = "<" ++ (show x) ++ (show (Fila xs))

    -- Fila segunda implementação (Resposta da questão)
  -- Definições das funções
  module Queue (Queue, enqueue, dequeue, front, queueEmpty, newQueue) where
  -- data Queue t = Fila [t] [t] deriving (Show)
  enqueue :: t -> Queue t -> Queue t
  dequeue :: Queue t -> Queue t
  front :: Queue t -> t
  queueEmpty :: Queue t -> Bool
  newQueue :: Queue t
  data Queue t = Fila [t] [t]

  -- Implementação otimizada
  newQueue = Fila [ ] [ ]

  queueEmpty (Fila [ ] [ ]) = True
  queueEmpty _ = False

  front (Fila [ ] [ ]) = error "A fila esta vazia"
  front (Fila (x:xs) _) = x
  front (Fila [ ] ys) = front (Fila (reverse ys) [ ])

  enqueue y (Fila [ ] [ ]) = (Fila [y] [ ])
  enqueue y (Fila [ ] ys) = (Fila [ ] (y : ys))
  enqueue y (Fila (x : xs) ys) = (Fila (x : xs) (y : ys))

  dequeue (Fila (x : xs) ys) = Fila xs ys
  dequeue (Fila [ ] [ ]) = error "A fila esta vazia"
  dequeue (Fila [ ] ys) = (Fila (tail (reverse ys)) [ ])

-- Questão 4
  -- Expressão ZF: 𝑍𝐹 ∶ 𝑥 = [(𝑎, 𝑏)|𝑎 < −[1. .4], 𝑏 < −[1. .4], 𝑎 > 𝑏

  -- Aplicando Lazy Evaluation
{-
  𝑍𝐹: 𝑥 = [(𝑎, 𝑏)|, 𝑎 < −[1. .4], 𝑏 < −[1. .4], 𝑎 > 𝑏]
  [(1, 𝑏)|, 𝑎 < −[1. .4], 𝑎
  > 𝑏] + +[(2, 𝑏)|, 𝑏 < −[1. .4], 𝑎 > 𝑏] + +[(3, 𝑏)|, 𝑏 < −[1. .4], 𝑎 > 𝑏]
  + +[(4, 𝑏)|, 𝑏 < −[1. .4], 𝑎 > 𝑏]
  = [(1, 1)|, 𝑎 > 𝑏] + +[(1, 2)|, 𝑎 > 𝑏] + +[(1, 3)|, 𝑎 > 𝑏] + +[(1, 4)|, 𝑎 > 𝑏]
  + +[(2, 1)|, 𝑎 > 𝑏] + +[(2, 2)|, 𝑎 > 𝑏] + +[(2,3)|, 𝑎 > 𝑏]
  + +[(2, 4)|, 𝑎 > 𝑏] + +[(3, 1)|, 𝑎 > 𝑏] + +[(3, 2)|, 𝑎 > 𝑏]
  + +[(3, 3)|, 𝑎 > 𝑏] + +[(3, 4)|, 𝑎 > 𝑏] + +[(4, 1)|, 𝑎 > 𝑏]
  + +[(4, 2)|, 𝑎 > 𝑏] + +[(4, 3)|, 𝑎 > 𝑏] + +[(4, 4)|, 𝑎 > 𝑏]
  = [(2, 1), (3, 1), (3, 2), (4, 1), (4, 2), (4, 3)]
-}

-- Questão 5
  -- Aplicando Lazy Evaluation
{-

  ZF: x = [a+b | a <- [1, 2], b <- [a..2*a]]

  - Resposta:
    x = [a+b | a <- [1, 2], b <- [a.. 2 * a]]
    x = [a+b | a <- [a, 2], b <- [1, 2]] ++ [a+b | a <- [1, a], b<- [2, 3, 4]]
    x = [1+b | , b <- [1, 2] ] ++ [2+b | b <- [2, 3, 4]]
    x = [1+1 | ] ++ [1+2 | ] ++ [2+2 | ] ++ [2+3 | ] ++ [2+4 | ]
    x = [2] ++ [1+2 | ] ++ [2+2 | ] ++ [2+3 | ] ++ [2+4 | ]
    x = [2] ++ [3] ++ [2+2 | ] ++ [2+3 | ] ++ [2+4 | ]
    x = [2] ++ [3] ++ [4] ++ [2+3 | ] ++ [2+4 | ]
    x = [2] ++ [3] ++ [4] ++ [5] ++ [2+4 | ]
    x = [2] ++ [3] ++ [4] ++ [5] ++ [6] 
    x = [2, 3, 4, 5, 6]
-}

-- Questão 6
  --Necessidade de usar descritores
{-
  São bons para melhorar a eficiência, já que toda vez que o comando writeFile
  ou appendFile for executado, acontece uma sequência de ações, onde o arquivo
  deve ser inicialmente aberto, a String deve ser escrita e, finalmente, o
  arquivo deve se fechado.
  Se muitas operações de escrita forem necessárias, haverá muitas
  destas sequências e isto implica em desempenho fraco. Ao se utilizar
  descritores, apenas uma operação de abertura no início e outra de
  fechamento ao final são necessárias.
-}

-- Questão 7

  -- Jeito do Max
  getAluno :: IO ()
  getAluno = do
    putStr "Digite o nome do aluno>> "
    nome <- getLine
    putStr "Digite a nota 1 do aluno>> "
    nota1 <- getLine
    putStr "Digite a nota 2 do aluno>> "
    nota2 <- getLine
    putStr "Digite a nota 3 do aluno>> "
    nota3 <- getLine
  
    let media = ((read nota1 :: Float) + (read nota2 :: Float) + (read nota3 :: Float))/3
    let show_media = show media
  
    putStrLn "\nInformações"
    putStr "- Nome: "
    putStrLn nome
    putStr "- Média: "
    putStrLn show_media
    putStr "\n"
  
    putStr "Pressione ENTER para continuar ou digite FIM para sair>> "
    resp <- getLine
    if (resp == "FIM")
      then return ()
      else getAluno

  -- Jeito dos slides e do mestre Vivi
  leiaAte :: IO ()
  leiaAte = do
    nome <- getLine
    if nome == “Final” then return ()
    else do
      n1 <- getDouble
      n2 <- getDouble
      n3 <- getDouble
      putStr (nome ++ show ((n1+n2+n3)/3.0) ++ “\n”)
      leiaAte

-- Questão 8
  -- OBS: Tá na questão 1 do jeito do Max, mas vou repetir essa porra

  -- Letra a (Calcular a profundidade de uma árvore de busca binária do tipo t)
  profArvBusBin :: ArvBusBin t -> Int -- maior distância de uma folha à raiz
  profArvBusBin (Nil) = 0
  -- profArvBusBin (No Nil n Nil) = 0
  profArvBusBin (No xt _ yt) = 1 + max (profArvBusBin xt) (profArvBusBin yt)

  -- Letra b (Construir umas ABB de t a partir de uma lista de um tipo t)
  fazABB :: (Ord t) => [t] -> ArvBusBin t
  fazABB [ ] = Nil
  fazABB (x : xs) = No (fazABB ys) x (fazABB zs)
    where (ys, zs) = particao (<= x) xs

  particao :: (t -> Bool) -> [t] -> ([t], [t])
  particao p xs = (filter p xs, filter (not . p) xs)

  -- Letra c (Transformar uma ABB de um tipo t numa lista ordenada [t])
  arvBBtoLista :: (Ord t) => ArvBusBin t -> [t]
  arvBBtoLista Nil = []
  arvBBtoLista (No xt n yt) = arvBBtoLista xt ++ [n] ++ arvBBtoLista yt

  -- Letra d (Verificar se um determinado valor do tipo t pertence ou não á uma arbore ArvBusBin t)
  buscarElemento :: (Ord t) => t -> ArvBusBin t -> Bool
  buscarElemento _ (Nil) = False
  buscarElemento elem (No xt n yt)
    | elem == n = True
    | elem < n = buscarElemento elem xt
    | otherwise = buscarElemento elem yt

-- Questão 9 (Formas geométricas)

  data Forma = Circulo Float | Triangulo Float Float Float | Retangulo Float Float deriving (Show)

  calculaArea :: Forma -> Float
  calculaArea (Circulo raio) = pi * (raio*raio) -- Ou 3.14 * raio * raio
  calculaArea (Retangulo a1 a2) = a1*a2
  calculaArea (Triangulo a1 a2 a3) = do
    let p = (a1 + a2 + a3)/2
    let x = p*(p-a1)*(p-a2)*(p-a3)
    sqrt x

  calculaPerimetro :: Forma -> Float
  calculaPerimetro (Circulo raio) = 2 * pi * raio
  calculaPerimetro (Triangulo a1 a2 a3) = a1 + a2 + a3
  calculaPerimetro (Retangulo a1 a2) = 2*(a1 + a2)

-- Anotações

    -- Eficiência dos progranas funcionais
  {-
    Por default, Haskell usa lazy evaluation, mas também pode usar eager
    evaluation, conhecida como avaliação gulosa, equivalente a passagem
    de parâmetros por valor, o que torna os programas bastante rápidos e
    eficientes.
    • No entanto, a avaliação estrita pode não atingir a forma normal,
    mesmo que ela exista, enquanto a avaliação preguiçosa garante a sua
    atingibilidade. Isto é garantido pelo Teorema de Church-Rosser.
    • No entanto, a avaliação preguiçosa pode exigir mais passos de
    execução que a avaliação estrita.
    • Alguns compiladores realizam uma verificação da adequabilidade, ou
    não, da avaliação estrita, conhecida como strictness anylising.
    • A análise de desempenho pode dar resultados diferentes se for
    utilizada uma avaliação estrita ou uma avaliação preguiçosa
  -}

    -- Pilha primeira implementação
  module Stack(Stack, push, pop, top, stackEmpty, newStack) where
  push :: t -> Stack t -> Stack t
  pop :: Stack t -> Stack t
  top :: Stack t -> t
  stackEmpty :: Stack t -> Bool
  newStack :: Stack t
  data Stack t = EmptyStk | Stk t (Stack t)

  import Stack
  listaParaPilha :: [t] -> Stack t
  listaParaPilha [ ] = newStack
  listaParaPilha (x : xs) = push x (listaParaPilha xs)
  pilhaParaLista :: Stack t -> [t]
  pilhaParaLista s
  |stackEmpty s = [ ]
  |otherwise = (top s) : (pilhaParaLista (pop s))

  top EmptyStk = error “topo de pilha vazia”
  top (Stk x _) = x

  newStack = EmptyStk

  stackEmpty EmptyStk = True
  stackEmpty _ = False

  instance (Show t) => Show (Stack t) where
  show (EmptyStk) = “#”
  show (Stk x s) = (show x) ++ “|” ++ (show s)

  push x s = Stk x s

  pop EmptyStk = error “retirada de Pilha vazia”
  pop (Stk _ s) = s

    -- Pilha segunda implementação
    module Stack(Stack, push, pop, top, stackEmpty, newStack} where
      push :: t -> Stack t -> Stack t
      pop :: Stack t -> Stack t
      top :: Stack t -> t
      stackEmpty :: Stack t -> Bool
      newStack :: Stack t
      data Stack t = Stk [t]
  
    push x (Stk xs) = Stk (x : xs)

    pop (Stk [ ]) = error "retirada em pilha vazia"
    pop (Stk (_ : xs)) = Stk xs

    top (Stk [ ]) = error "topo de pilha vazia"
    top (Stk (x : _)) = x

    newStack = Stk [ ]

    stackEmpty (Stk [ ]) = True
    stackEmpty _ = False

    instance (Show t) => Show (Stack t) where

    show (Stk [ ]) = "#"
    show (Stk (x : xs)) = (show x) ++ "|" ++ (show (Stk xs))

    -- Tipo abstrato Set
  module Set (Set, emptySet, setEmpty, inSet, addSet, delSet) where
    emptySet :: Set t
    setEmpty :: Set t -> Bool
    inSet :: (Eq t) => t -> Set t -> Bool
    addSet :: (Eq t) => t -> Set t -> Set t
    delSet :: (Eq t) => t -> Set t -> Set t
    pickSet :: Set t -> t
    data Set t = S [t] --listas sem repetições

  emptySet = S [ ]

  setEmpty (S [ ]) = True
  setEmpty _ = False

  inSet _ (S [ ]) = False
  inSet x (S (y : ys))
    |x == y = True
    |otherwise = inSet x (S ys)

  addSet x (S s)
    |(elem x s) = S s
    |otherwise = S (x : s)

  delSet x (S s) = S (delete x s)
  delete x [ ] = [ ]
  delete x (y : ys)
    |x == y = delete x ys
    |otherwise = y : (delete x ys)

  pickSet (S [ ]) = error "conjunto vazio"
  pickSet (S (x : _)) = x

    -- Tipo abstrato Table
  module Table (Table, newTable, findTable, updateTable, removeTable) where
    newTable :: Table a b
    findTable :: (Ord a) => a -> Table a b -> Maybe b
    updateTable :: (Ord a) => (a, b) -> Table a b -> Table a b
    removeTable :: (Ord a) => a -> Table a b -> Table a b
    data Table a b = Tab [(a,b)] --lista ordenada de forma crescente

  newTable = Tab [ ]

  findTable _ (Tab [ ]) = Nothing
  findTable x (Tab ((c,v) : resto))
    |x < c = Nothing
    |x == c = Just v
    |x > c = findTable x (Tab resto)

  updateTable (x, z) (Tab [ ]) = Tab [(x, z)]
  updateTable (x, z) (Tab ((c,v) : resto))
    |x < c = Tab ((x,z):(c,v):resto)
    |x == c = Tab ((c,z):resto)
    |x > c = let (Tab t) = updateTable (x,z) (Tab resto) in Tab ((c,v):t)

  removeTable _ (Tab [ ]) = Tab [ ]
  removeTable x (Tab ((c,v):resto))
    |x < c = Tab ((c,v):resto)
    |x == c = Tab resto
    |x > c = let (Tab t) = removeTable x (Tab resto) in Tab ((c,v):t)

  instance (Show a, Show b) => Show (Table a b) where
    show (Tab [ ]) = " "
    show (Tab ((c,v):resto))
    = (show c)++"\t"++(show v)++"\n"++(show (Tab resto))


    -- Ações ----------------------------------------------------
{-
  É um tipo especial de dado em Haskell. Quando o sistema avalia uma expressão cujo
  resultado seja uma ação, ele sabe que não deve mostrar este resultado no dispositivo
  padrão de saída e sim, apenas executar a ação apropriada.
  • Ações primitivas
  Escrever um caractere em um arquivo ou recebê-lo via teclado.
  • Ações compostas
  Imprimir uma String inteira em um arquivo
  • Comandos
  Funções em Haskell, cujos resultados de suas avaliações sejam ações
-}

  faz4vezes :: String -> IO ( )
  faz4vezes str = do  putStrLn str
                      putStrLn str
                      putStrLn str
                      putStrLn str

  fazNvezes :: Int -> String -> IO ( )
  fazNvezes n str = if n <= 1
                      then putStrLn str
                      else do
                        putStrLn str
                        fazNvezes (n-1) str
                        e faz faz4vezes = fazNvezes 4

  -- Arquivos
  import System.IO
  import Data.Char (toUpper)
  main :: IO ( )
  main = do entrada <- openFile “entra.txt” ReadMode
  saída <- openFile “sai.txt” WriteMode
  loop_principal entrada saida
  hClose entrada
  hClose saída
  loop_principal :: Handle -> Handle -> IO ( )
  loop_principal entrada saída =
  do fim_de_arquivo <-hIsEOF entrada
  if fim_de_arquivo then return ( )
  else do inpStr <- hGetLine entrada
  hPutStrLn saída (map toUpper inpStr)
  loop_principal entrada saida