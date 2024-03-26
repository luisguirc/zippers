# Zippers: propósito, funcionamento, e mini-jogo TimeZipper
## Introdução e motivação
Imagine que você está em um labirinto, tentando encontrar uma forma de sair. Você segue por um caminho, escolhendo aleatoriamente a cada cruzamento, até que se depara com uma seção sem saída. Digamos que, neste momento, você deseja voltar para a última bifurcação (ou divergência) para seguir por outro caminho. Seria inconveniente (e exigiria um grande tempo) ter que reiniciar o labirinto, repetindo cada passo que você deu do início até chegar neste cruzamento desejado, para então seguir por outro corredor.

É muito mais interessante neste caso possuir um histórico de cada passo que foi tomado, e um contexto: uma forma rápida de retornar a um estado anterior do problema (neste caso, o momento em que você se encontrava no cruzamento que deseja voltar). Usualmente, esse histórico e contexto estão em nosso cérebro, mas podemos usar mecanismos para construir esta estrutura.

Na computação, este conceito recebe o nome de Zipper. Este nome é inspirado no mecanismo usado em mochilas e roupas: percorrer uma lista de dados deslocando o nosso foco ao longo de sua extensão lembra muito o funcionamento dos zíperes de uma calça.

## Estrutura geral e funcionamento
Um Zipper possui um "foco": uma parte da estrutura de dados que está em evidência, e um contexto, que armazena o resto da estrutura do ponto de vista deste foco. Isso pode ser útil para percorrer (caminhar por) a estrutura, enquanto aplica transformações em cada um de seus nós (ou qualquer outra nomenclatura para a unidade de uma dada estrutura). Em Haskell, isso pode ser traduzido como uma tupla da seguinte forma:
```
data Zipper a = (Focus a, Context a)
```
Sendo _Focus_ e _Context_ dependente da implementação da estrutura de dados. Ou, ainda
```
data Zipper f g a = Zipper (f a) (g a)
```
Que é mais genérica, e nos permitirá adaptar o conceito geral de um Zipper para diferente estruturas de dados. Esta última será a definição utilizada por este tutorial.

### Em listas
Antes de tratarmos de casos mais complexos, vamos nos ater a uma das estruturas de dados mais simples: a lista. Em Haskell, uma lista ligada _lista_ = [1, 2, 3, 4] pode ser vista como:
```
let lista = 1 : 2 : 3 : 4 : []
```
Podemos construir uma função que percorre a lista, trazendo o foco para o próximo elemento:
```
passoParaFrente :: [a] -> [a]
passoParaFrente []     = []
passoParaFrente [x:xs] = xs
```
Ao executar passoParaFrente em _lista_, temos como retorno:
```
ghci> passoParaFrente lista
2 : 3 : 4 : []
```
E nos encontraríamos na mesma situação do labirinto: não há forma voltar ao estado em que o foco é o elemento "1" (executando um "passo para trás") exceto começando novamente da lista inicial.
Para resolver isso com um Zipper (levando em conta a definição acima), definimos o seguinte apelido de tipo:
```
type ListaZipper a = Zipper [] [] a
```
Sendo:
- Primeira lista: contexto;
- Segunda lista: o primeiro elemento será o foco, seguido do resto da lista.

Podemos ainda implementar uma função que recebe uma lista comum, e constrói um Zipper:
```
criarListaZipperDeLista :: [a] -> ListaZipper a
criarListaZipperDeLista = Zipper []
```

> Não é necessário manipular a lista como um argumento, já que, em ambos os lados da atribuição, a lista é o último argumento.

Essa implementação é possível pois, inicialmente, o foco do Zipper é o primeiro elemento da lista.
Assim, definimos novas funções de _passoParaFrente_ e _passoParaTras_:
```
passoParaFrente :: ListaZipper a -> ListaZipper a
passoParaFrente z@(Zipper _ [])               = z
passoParaFrente   (Zipper contexto (foco:xs)) = Zipper (foco : contexto) xs
```
```
passoParaTras :: ListaZipper a -> ListaZipper a
passoParaTras z@(Zipper [] _)           = z
passoParaTras   (Zipper (c:contexto) xs) = Zipper contexto (c : xs)
```
Note que ainda poderia ser definida uma função de transformação, que recebe uma função e aplica no foco.
```
transformar :: (a -> a) -> ListaZipper a -> ListaZipper a
transformar _ z@(Zipper _ []) = z
transformar f   (Zipper c (foco:xs)) = Zipper c (f foco:xs)
```
Ou ainda funções que deletam um elemento, ou inserem um novo antes do foco (tornando-o o novo foco):
```
deletarFoco :: ListaZipper a -> ListaZipper a
deletarFoco z@(Zipper _ [])     = z
deletarFoco   (Zipper c (f:xs)) = Zipper c xs
```
```
inserir :: a -> ListaZipper a -> ListaZipper a
inserir x (Zipper c xs) = Zipper c (x:xs)
```
### Em árvores binárias
Agora, vamos olhar para árvores binárias. Uma árvore é uma estrutura de nós ligados. No caso de uma árvore binária, cada nó possui no máximo dois nós filhos. Em Haskell, podemos representar esta estrutura desta forma:
```
data Arvore a = Vazio | No (Arvore a) a (Arvore a)
```
Vamos relembrar nossa definição genérica de um Zipper:
```
data Zipper f g a = Zipper (f a) (g a)
```
Portanto, nosso foco será um nó desta árvore, e o contexto será o passo a passo de como a árvore foi percorrida a partir da raiz para chegar ao nó foco. Precisa-se ter no contexto informação suficiente para reconstruir a árvore original: se o fluxo seguido foi o nó da esquerda ou da direita, quais os nós da sub-árvore que não foi percorrida, etc.

Desta forma, , usaremos de uma nova estrutura chamada *Passo*, que identificará a direção que seguimos pela árvore, o nó raiz antes de seguirmos pela subárvore  (anteriormente o foco do Zipper), e a outra subárvore não percorrida:
```
data Passo a = Esq a (Arvore a) | Dir a (Arvore a)
```
O tipo *Passos* será a forma de armazenarmos este caminho: o conjunto de passos até um ponto x da árvore.
```
newtype Passos a = Passos [Passo a]
```
Por fim, o apelido de tipo para nossa *ArvoreZipper* será:
```
type ArvoreZipper a = Zipper Arvore Passos a
```
Assim como fizemos com o *ListaZipper*, podemos criar uma função que transforma uma lista comum em um *ArvoreZipper*.

> Para isso, consideramos que o i-ésimo nó da lista tem seu filho esquerdo na posição 2*i + 1 da lista, e seu filho direito na posição 2*i + 2.

```
criarArvoreZipperDeLista :: [a] -> ArvoreZipper a
criarArvoreZipperDeLista []    = Zipper Vazio (Passos [])
criarArvoreZipperDeLista lista = Zipper (criarArvore 0) (Passos [])
  where
    criarArvore i
      | i > length lista - 1 = Vazio
      | otherwise = No (criarArvore (2 * i + 1)) (lista !! i) (criarArvore (2 * i + 2))
```
Podemos também definir movimentos na árvore: seguir pelo nó filho esquerdo ou direto, ou ainda retornar ao nó pai. Note que, por enquanto, estamos apenas não realizando nenhuma ação caso não seja possível seguir pelo caminho escolhido (executar *subir* em um nó raiz, ou *irParaDireita* em um nó folha). Isso será resolvido com *Maybe* mais adiante.
```
irParaEsquerda :: ArvoreZipper a -> ArvoreZipper a
irParaEsquerda z@(Zipper Vazio _)                = z
irParaEsquerda z@(Zipper (No Vazio _ _) _)       = z
irParaEsquerda   (Zipper (No l x r) (Passos ps)) = Zipper l (Passos (Esq x r:ps))
```
```
irParaDireita :: ArvoreZipper a -> ArvoreZipper a
irParaDireita z@(Zipper Vazio _)                = z
irParaDireita z@(Zipper (No _ _ Vazio) _)       = z
irParaDireita   (Zipper (No l x r) (Passos ps)) = Zipper r (Passos (Dir x l:ps))
```
```
subir :: ArvoreZipper a -> ArvoreZipper a
subir z@(Zipper _ (Passos []))           = z
subir   (Zipper t (Passos (Esq x r:ps))) = Zipper (No t x r) (Passos ps)
subir   (Zipper t (Passos (Dir x l:ps))) = Zipper (No l x t) (Passos ps)
```
É possível definir uma função que aplica algum tipo de transformação no nó foco:
```
modificar :: (a -> a) -> ArvoreZipper a -> ArvoreZipper a
modificar _ z@(Zipper Vazio _)                = z
modificar f   (Zipper (No l x r) (Passos ps)) = Zipper (No l (f x) r) (Passos ps)
```
Ou ainda usar da definição de subir para construir uma função que volta ao início da árvore, ou algum ponto específico do caminho entre raiz e nó atual:
```
irAoTopo :: ArvoreZipper a -> ArvoreZipper a
irAoTopo z@(Zipper _ (Passos [])) = z
irAoTopo z                        = irAoTopo (subir z)
```
```
voltarParaPonto :: Eq a => a -> ArvoreZipper a -> ArvoreZipper a
voltarParaPonto _ z@(Zipper Vazio _)       = z
voltarParaPonto _ z@(Zipper _ (Passos [])) = z
voltarParaPonto y z@(Zipper n@(No _ x _) (Passos _))
  | x /= y = voltarParaPonto y (subir z)
  | otherwise = z
```
Com a nossa implementação, uma função de anexar nós fica muito simples:
```
anexar :: a -> ArvoreZipper a -> ArvoreZipper a
anexar x   (Zipper Vazio ps)      = Zipper (No Vazio x Vazio) ps
anexar _ z@(Zipper _ _)           = z
```
Basta substituir o nó atual (caso seja vazio) por um nó com o elemento x que queremos anexar. Caso contrário, retornamos o *ArvoreZipper* sem alterações. Também podemos fazer uso de funções mais simples para conseguir facilmente o foco de um ArvoreZipper, e seu elemento raiz:
```
getFoco :: ArvoreZipper a -> Arvore a
getFoco (Zipper foco _) = foco
```
```
getElemento :: Arvore a -> a
getElemento (No _ x _) = x
```

#### Usando Maybe
Até agora, precisamos sempre usar de Pattern Matching em nossas funções para garantir que não estaríamos gerando algum erro ao tentar irParaDireita a partir de um nó vazio da árvore. Para isso, sempre garantimos que tentar percorrer a árvore a partir de um nó Vazio (ou em direção a um nó vazio) na verdade simplesmente retorna o Zipper sem nenhuma modificação.

Outra abordagem é fazer uso de *Maybe*, que nos ajudará a lidar com a possibilidade de falha ao percorrer a árvore.

*Maybe* encapsula um valor opcional, ou seja, é definido como:
```
data Maybe a = Just a | Nothing
```


Aqui estão as funções de movimentação utilizando de *Maybe*:

```
irParaEsquerdaM :: ArvoreZipper a -> Maybe (ArvoreZipper a)
irParaEsquerdaM (Zipper (No l x r) (Passos bs)) = Just (Zipper l (Passos (Esq x r : bs)))
irParaEsquerdaM (Zipper Vazio _)                = Nothing
```
```
irParaDireitaM :: ArvoreZipper a -> Maybe (ArvoreZipper a)
irParaDireitaM (Zipper (No l x r) (Passos bs)) = Just (Zipper r (Passos (Dir x l : bs)))
irParaDireitaM (Zipper Vazio _)                = Nothing
```
```
subirM :: ArvoreZipper a -> Maybe (ArvoreZipper a)
subirM (Zipper t (Passos (Esq x r : bs))) = Just (Zipper (No t x r) (Passos bs))
subirM (Zipper t (Passos (Dir x l : bs))) = Just (Zipper (No l x t) (Passos bs))
subirM (Zipper _ (Passos []))             = Nothing
```

No entanto, estas funções agora recebem *ArvoreZipper a*, mas retornam *Maybe (ArvoreZipper a)*. Desta forma, não podemos encadeá-las como anteriormente. Isso gera o seguinte erro:
```
ghci> (irParaEsquerdaM (irParaEsquerdaM arvore))

<interactive>:26:19: error:
    • Couldn't match type: Maybe (ArvoreZipper a0)
                     with: Zipper Arvore Passos a
```
Para resolver isso, faremos uso do operador **bind (>>=)**. Os conceitos relacionados ao *bind* poderiam ser um tutorial por si só, portanto vamos nos ater ao que ele nos trará de benefício.
O tipo de >>= é definido como:
```
ghci> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```
Portanto, iremos passar como argumento ao operador bind um *Maybe (ArvoreZipper a)*, e uma função do tipo *ArvoreZipper a -> Maybe (ArvoreZipper a)*. Na prática, temos:
```
ghci> let arvore = criarArvoreZipperDeLista [1, 2, 3, 4]
(para facilitar visualização)
    1
  2   3
4

ghci> return arvore >>= irParaEsquerdaM
Just (Zipper (No (No Vazio 4 Vazio) 2 Vazio) (Passos [Esq 1 (No Vazio 3 Vazio)]))

ghci> return arvore >>= irParaEsquerdaM >>= irParaDireitaM
Just (Zipper Vazio (Passos [Dir 2 (No Vazio 4 Vazio),Esq 1 (No Vazio 3 Vazio)]))

ghci> return arvore >>= irParaEsquerdaM >>= irParaDireitaM >>= irParaDireitaM
Nothing
```
Assim, quando houver qualquer "falha" (alguma função retornar *Nothing* ao longo do percurso), o resultado de toda o percurso também será *Nothing*.

## Caso de uso prático: TimeZipper
Antes de seguir adiante, é recomendado tentar rodar o jogo por conta própria! Assim, você vai entender um pouco mais do seu funcionamento. Você pode baixar o arquivo e executá-lo com o GHCI, ou acessar o [Replit](https://replit.com/@luisguirc/zipper?v=1) para executá-lo em uma VM.
Você também pode checar o arquivo deste tutorial neste [link](https://github.com/luisguirc/zippers/blob/main/Main.hs).

### Definições
Vamos, por fim, construir um jogo de decisões baseado na implementação em árvore de um Zipper. Este pode ser categorizado como um [livro-jogo](https://pt.wikipedia.org/wiki/Livro-jogo), uma história ficcional que se baseia em decisões do leitor. Tais decisões acabam por definir o fluxo da história.

Para isso, vamos começar com algumas definições. Como queremos construir uma história, precisamos de uma árvore de **Situações**. Portanto, definimos:
```
data Situacao = MKSituacao 
    { _descricao  :: String
    , _opcao1     :: String
    , _opcao2     :: String
    , _ehFinal    :: Bool
    , _resultado  :: Maybe Resultado
    , _id         :: Char
    } deriving (Show, Eq)
```
Onde, em cada situação, teremos uma descrição, duas opções apresentadas ao usuário, um identificador, e uma forma de entender se esta situação é final ou não, e. caso seja, se é um sucesso ou uma falha. Por isso usamos do *Maybe*: só nos interessa o resultado se a situação é final.
```
data Resultado = Sucesso | Falha deriving (Show, Eq)
```
Precisaremos definir cada uma das situações usando seu construtor. Por exemplo:
```
s0 :: Situacao
s0 = MKSituacao "Uma pessoa encapuzada sai correndo de dentro, com o que parece ser um saco de tecido por cima de seu ombro, e algumas cédulas pairando e caindo de seu interior. Você precisa tomar sua primeira decisão."
				"Entrar em seu carro e perseguí-la rua abaixo."
				"Correr atrás da pessoa."
				False
				Nothing
				'A'
```

### Execução

> As funções aqui contidas estão simplificadas - com menos impressão de texto na tela, principalmente - para melhor compreensão de seu funcionamento. Para funções completas, acesse o [arquivo do código](https://github.com/luisguirc/zippers/blob/main/Main.hs).

Com as definições, construiremos o Zipper do enredo do jogo (de tipo ArvoreZipper Situacao) usando o arcabouço de funções que já definimos para árvores binárias. Podemos construir o Zipper usando uma lista com todas as situações e a função *criarArvoreZipperDeLista*, ou ainda usar de *irParaEsquerda* ou *irParaDireita* para navegar pelas escolhas do usuário.

Para iniciar nosso jogo, usaremos da função *iniciarJogo*
```
iniciarJogo :: IO()
iniciarJogo = do
	putStrLN "introdução ao jogo não inserida neste tutorial, apenas no arquivo de código"
    let arvore = criarArvoreZipperDeLista [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18]
    lacoDoJogo arvore
```
O coração do nosso jogo será a função *lacoDoJogo*, definida abaixo.
Como *criarArvoreZipperDeLista* define o foco como o primeiro elemento da lista, *lacoDoJogo* tem em sua primeira chamada recursiva o foco como a primeira situação do jogo.
```
lacoDoJogo :: ArvoreZipper Situacao -> IO ()
lacoDoJogo estado = do
  entrada <- receberEscolha estado
  proximoEstado <- processarEntrada entrada estado
  (ehFinal, resultadoFinal) <- fimDeJogo proximoEstado
  if ehFinal
    then case resultadoFinal of
      Just Sucesso -> do
        putStrLn "Sucesso! Jogo encerrado :)"
      Just Falha -> do
        estadoFinal <- processarEntradaFinal proximoEstado
        lacoDoJogo estadoFinal
      Nothing -> error "Chegou ao estado final, mas Resultado não está definido."
    else lacoDoJogo proximoEstado
```
Essa função tem várias coisas a serem descritas. Mas em geral, o seu fluxo é:

- Imprima na tela a descrição da situação e peça input do usuário sobre a opção escolhida;
- Gere o próximo estado usando do Zipper e do input do usuário;
- Caso seja um estado final de sucesso, termine o jogo;
- Caso seja um estado final de falha, ofereça ao jogador voltar a uma decisão anterior e continuar o jogo;
- Se o estado for final, mas não for uma falha nem sucesso, lance um erro.
- Caso não seja um estado final, chame recursivamente *lacoDoJogo* com o próximo estado de acordo com a escolha do jogador.

Portanto, temos uma melhoria a ser feita na definição de Situacao, a fim de melhorar nossas restrições em relação a estados que são finais, e que são de sucesso ou falha.
Portanto, temos essas novas definições: (e não precisaremos mais do *Maybe*)
```
data ConteudoSituacao
    = Final { _resultado :: Resultado }
    | NaoFinal
    deriving (Show, Eq)

data Situacao = MKSituacao 
    { _descricao           :: String
    , _opcao1              :: String
    , _opcao2              :: String
    , _conteudoSituacao    :: ConteudoSituacao
    , _id                  :: Char
    }
    deriving (Show, Eq)
```
Portanto, em _conteudoSituacao teremos apenas as opções (Final Falha), (Final Sucesso), e NaoFinal.
Caso tenha interesse em entender mais sobre o funcionamento do jogo, acesse o [arquivo de código](https://github.com/luisguirc/zippers/blob/main/Main.hs).

Espero que tenha gostado e se interessado pelos Zippers neste breve tutorial!
Como aprendido, não desperdice seu tempo ;)

Luis Crosselli
UFABC
