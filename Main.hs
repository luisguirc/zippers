module Main where
import System.IO
import Data.List (intercalate)

data Zipper f g a = Zipper (f a) (g a) deriving Show

--------------Zipper em Listas------------------------------------------

type ListaZipper a = Zipper [] [] a

criarListaZipperDeLista :: [a] -> ListaZipper a
criarListaZipperDeLista = Zipper []

passoParaFrente :: ListaZipper a -> ListaZipper a
passoParaFrente z@(Zipper _ [])               = z
passoParaFrente   (Zipper contexto (foco:xs)) = Zipper (foco : contexto) xs

passoParaTras :: ListaZipper a -> ListaZipper a
passoParaTras z@(Zipper [] _)           = z
passoParaTras   (Zipper (c:contexto) xs) = Zipper contexto (c : xs)

transformar :: (a -> a) -> ListaZipper a -> ListaZipper a
transformar _ z@(Zipper _ []) = z
transformar f   (Zipper c (foco:xs)) = Zipper c (f foco:xs)

inserir :: a -> ListaZipper a -> ListaZipper a
inserir x (Zipper c xs) = Zipper c (x:xs)

deletarFoco :: ListaZipper a -> ListaZipper a
deletarFoco z@(Zipper _ [])     = z
deletarFoco   (Zipper c (f:xs)) = Zipper c xs

------------Ziper em Árvores Binárias-----------------------------------

data Arvore a = Vazio | No (Arvore a) a (Arvore a) deriving Show

data Passo a = Esq a (Arvore a) | Dir a (Arvore a) deriving Show

newtype Passos a = Passos [Passo a] deriving Show

type ArvoreZipper a = Zipper Arvore Passos a

irParaEsquerda :: ArvoreZipper a -> ArvoreZipper a
irParaEsquerda z@(Zipper Vazio _)                = z
irParaEsquerda z@(Zipper (No Vazio _ _) _)       = z
irParaEsquerda   (Zipper (No l x r) (Passos ps)) = Zipper l (Passos (Esq x r:ps))

irParaDireita :: ArvoreZipper a -> ArvoreZipper a
irParaDireita z@(Zipper Vazio _)                = z
irParaDireita z@(Zipper (No _ _ Vazio) _)       = z
irParaDireita   (Zipper (No l x r) (Passos ps)) = Zipper r (Passos (Dir x l:ps))

subir :: ArvoreZipper a -> ArvoreZipper a
subir z@(Zipper _ (Passos []))           = z
subir   (Zipper t (Passos (Esq x r:ps))) = Zipper (No t x r) (Passos ps)
subir   (Zipper t (Passos (Dir x l:ps))) = Zipper (No l x t) (Passos ps)

criarArvoreZipperDeLista :: [a] -> ArvoreZipper a
criarArvoreZipperDeLista []    = Zipper Vazio (Passos [])
criarArvoreZipperDeLista lista = Zipper (criarArvore 0) (Passos [])
  where
    criarArvore i
      | i > length lista - 1 = Vazio
      | otherwise = No (criarArvore (2 * i + 1)) (lista !! i) (criarArvore (2 * i + 2))

modificar :: (a -> a) -> ArvoreZipper a -> ArvoreZipper a
modificar _ z@(Zipper Vazio _)                = z
modificar f   (Zipper (No l x r) (Passos ps)) = Zipper (No l (f x) r) (Passos ps)

irAoTopo :: ArvoreZipper a -> ArvoreZipper a
irAoTopo z@(Zipper _ (Passos [])) = z
irAoTopo z                        = irAoTopo (subir z)

voltarParaPonto :: Eq a => a -> ArvoreZipper a -> ArvoreZipper a
voltarParaPonto _ z@(Zipper Vazio _)       = z
voltarParaPonto _ z@(Zipper _ (Passos [])) = z
voltarParaPonto y z@(Zipper n@(No _ x _) (Passos _))
  | x /= y = voltarParaPonto y (subir z)
  | otherwise = z

anexar :: a -> ArvoreZipper a -> ArvoreZipper a
anexar x   (Zipper Vazio ps)      = Zipper (No Vazio x Vazio) ps
anexar _ z@(Zipper _ _)           = z

getFoco :: ArvoreZipper a -> Arvore a
getFoco (Zipper foco _) = foco

getElemento :: Arvore a -> a
getElemento (No _ x _) = x

------------Usando Maybe------------------------------------------------

irParaEsquerdaM :: ArvoreZipper a -> Maybe (ArvoreZipper a)
irParaEsquerdaM (Zipper (No l x r) (Passos bs)) = Just (Zipper l (Passos (Esq x r : bs)))
irParaEsquerdaM (Zipper Vazio _)                = Nothing

irParaDireitaM :: ArvoreZipper a -> Maybe (ArvoreZipper a)
irParaDireitaM (Zipper (No l x r) (Passos bs)) = Just (Zipper r (Passos (Dir x l : bs)))
irParaDireitaM (Zipper Vazio _)                = Nothing

subirM :: ArvoreZipper a -> Maybe (ArvoreZipper a)
subirM (Zipper t (Passos (Esq x r : bs))) = Just (Zipper (No t x r) (Passos bs))
subirM (Zipper t (Passos (Dir x l : bs))) = Just (Zipper (No l x t) (Passos bs))
subirM (Zipper _ (Passos []))             = Nothing

----------------Jogo: ZipperTime----------------------------------------

data Resultado = Sucesso | Falha deriving (Show, Eq)

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

s0 :: Situacao
s0 = MKSituacao "Uma pessoa encapuzada sai correndo de dentro, com o que parece ser um saco de tecido por cima de seu ombro, e algumas cédulas pairando e caindo de seu interior. Você precisa tomar sua primeira decisão." "Entrar em seu carro e perseguí-la rua abaixo." "Correr atrás da pessoa." NaoFinal 'A'

s1 :: Situacao
s1 = MKSituacao "Você entra em seu carro, e rapidamente o liga. Você dirige extremamente rápido, cortando outros carros nesta via pouco movimentada, até que a pessoa encapuzada olha para trás, e percebe que você está no carro, e perseguindo-a. Você também repara que se trata de uma menina não muito velha. Assim, ela entra correndo em uma viela que seu carro não conseguirá percorrer." "Descer do carro e continuar a perseguição a pé." "Dar a volta no quarteirão." NaoFinal 'B'

s2 :: Situacao
s2 = MKSituacao "Você corre atrás da pessoa, e das células que voam por cima de seu ombro. Durante a perseguição, nota que se trata de uma menina, provavelmente com menos de 20 anos. Ela corre muito, passando por pequenas aglomerações na calçada, e você segue em seu encalço. Você começa eventualmente a se cansar" "Gritar pela menina." "Gritar para que as pessoas na calçada a segurem." NaoFinal 'C'

s3 :: Situacao
s3 = MKSituacao "Após descer de seu carro, você percorre a viela, tentando manter a menina sempre a vista, o que não é difícil, já que se pode seguir o rastro das cédulas no chão. Após virar em uma quina do beco, você não vê mais a pessoa, mas há uma escada à sua direita, e uma porta à sua esquerda. Você deve decidir por onde vai seguir." "Subir a escada." "Entrar pela porta." NaoFinal 'D'

s4 :: Situacao
s4 = MKSituacao "Você dá a volta, e para na única saída da viela. Você espera, espera, e espera. Até que percebe que já se passou muito tempo, e que ela deveria ter aparecido. Infelizmente, você perdeu seu rastro." "" "" (Final Falha) 'E'

s5 :: Situacao
s5 = MKSituacao "Você grita: \"EI! Pare já! Vão atrás de você, você não vai conseguir se esconder!\"\nEnquanto corre, ela se vira, e dá risadas na sua direção. Eventualmente, ela entra em uma viela estreita, e você corre atrás dela.\nVocê passa por escadas, portas suspeitas do que é provavelmente os fundos de algum estabelecimento, e se cansa cada vez mais.\nEventualmente, ela larga no ar o que parece ser um pequeno bilhete, que passa perto o suficiente de seus olhos para que você tenha um vislumbre de seu conteúdo, e isso te espanta." "Parar de correr e inspecionar o bilhete." "Continuar correndo" NaoFinal 'F'

s6 :: Situacao
s6 = MKSituacao "Você grita: \"PEGUEM ELA! Ela acabou de roubar um banco! Peguem a menina!\". As várias pessoas que estão na calçada à frente de você ficam agitadas, e começam a tentar agarrar a menina. Ela é agil, e desvia de todos, causando um alvoroço. Você, muito menos ágil que ela, acaba trombando em outras pessoas, até que já não a enxerga mais. Você sabe que ficou para trás. E terá que Zippar" "" "" (Final Falha) 'G'

s7 :: Situacao
s7 = MKSituacao "A escada sobe pela parede de um prédio. Ao chegar no topo do edifício, que é um pátio amplo e plano, você se depara com caixas e vasos de plantas, e uma senhora sentada em uma cadeira de balanço de madeira lendo um livro. Você se aproxima dela." "\"Você viu uma menina correndo por aqui, com um saco cheio de dinheiro?\"" "\"Droga! Perdi ela de vista. Terei que Zippar.\"" NaoFinal 'H'

s8 :: Situacao
s8 = MKSituacao "A porta dá em um pequeno moquifo com música tocando ao fundo do som de muitas conversas. Seus olhos se acostumam com a baixa luminosidade, e você identifica o que parece ser um bar bem cheio. uma mulher de meia idade do outro lado do cômodo está claramente notando como você está perdido." "Procurar por alguém que te lembre a menina que saiu do banco." "Questionar a mulher." NaoFinal 'I'

s9 :: Situacao
s9 = MKSituacao "" "" "" NaoFinal 'x'

s10 :: Situacao
s10 = MKSituacao "" "" "" NaoFinal 'x'

s11 :: Situacao
s11 = MKSituacao "Você para de correr, e volta para ver o que o bilhete contém. Na verdade, o papel é uma foto. Esta foto mostra você e algumas crianças, ao lado de uma pessoa que você não conhece, em alguma festividade. Você só parece com um pouco mais de idade... O que isso significa? Essa pessoa te conhece? Está fazendo montagens? Infelizmente, você a perdeu de vista." "" "" (Final Falha) 'J'

s12 :: Situacao
s12 = MKSituacao "Você corre, e corre, e corre. Mas parece que a menina não se cansa. Eventualmente, você sente que se continuar correndo, irá desmaiar. Você desiste da perseguição, e começa a voltar pela viela. Até que passa pelo bilhete que ela soltou, mas ele está se desfazendo em uma poça d'água." "" "" (Final Falha) 'K'

s13 :: Situacao
s13 = MKSituacao "" "" "" NaoFinal 'x'

s14 :: Situacao
s14 = MKSituacao "" "" "" NaoFinal 'x'

s15 :: Situacao
s15 = MKSituacao "A senhora levanta os olhos de seu livro e encontra os seus. Ela solta uma risada, e diz: \"Rapaz... Vi sim, junto com meu marido Jerimundo. Que saudade daquele velinho...\". Desta forma, você percebe que perdeu seu rastro." "" "" (Final Falha) 'L'

s16 :: Situacao
s16 = MKSituacao "A senhora levanta os olhos de seu livro, e eles parecem brilhar. Você nota uma cicatriz no lado esquerdo de sua face, que vai do topo da testa até a têmpora. Ela diz: \"Sabia que iria te encontrar aqui... já fazem tantos anos. Foi bom enquanto durou. Você me perseguiu por décadas, fez disso o motivo de sua vida, e eu, no fim, acabei nunca sendo pega. Sabendo dos meus momentos finais, acho que te devo essa. O dinheiro está ali\", e ela aponta para uma caixa com algumas cédulas penduradas para fora. \"Meu endereço nessa época é este. Você me encontrará lá. Só, por favor, use seu tempo melhor do que você usou comigo\". Você não entende muito bem o que aquela senhora quer dizer, mas olha para o livro em suas mãos, e lê seu título: \"Zippers: propósito, funcionamento, e mini-jogo TimeZipper\"" "" "" (Final Sucesso) 'M'

s17 :: Situacao
s17 = MKSituacao "Você tromba em várias pessoas, mas não a encontra. Sabe que perdeu ela de vez. Ao menos por enquanto." "" "" (Final Falha) 'N'

s18 :: Situacao
s18 = MKSituacao "Você chega próximo dela, e percebe que ela possui uma pequena cicatriz na lateral da testa.  Você pergunta: \"O que você está olhando? Perdeu alguma coisa?\". Ela abre um sorriso de canto de boca, e diz: \"Apenas vendo alguém desperdiçando seu tempo\". Você se arrepende de ter ido falar com ela, e agora já não sabe mais por qual caminho seguir, exceto Zippar." "" "" (Final Falha) 'P'


lacoDoJogo :: ArvoreZipper Situacao -> IO ()
lacoDoJogo estado = do
  entrada <- receberEscolha estado
  proximoEstado <- processarEntrada entrada estado
  (ehFinal, resultadoFinal) <- fimDeJogo proximoEstado
  if ehFinal
    then case resultadoFinal of
      Just Sucesso -> do
        putStrLn $ "[ID " ++ [_id s16] ++ "]. " ++ _descricao s16
        putStrLn "\nEste foi TimeZipper. Obrigado por jogar, e use bem seu tempo! :)"
      Just Falha -> do
        estadoFinal <- processarEntradaFinal proximoEstado
        lacoDoJogo estadoFinal
      Nothing -> error "Chegou ao estado final, mas Resultado não está definido."
    else lacoDoJogo proximoEstado

lerDoTeclado :: String -> IO Char
lerDoTeclado mensagem = do
  putStrLn mensagem
  hFlush stdout
  input <- getChar
  _ <- getLine
  return input

receberEscolha :: ArvoreZipper Situacao -> IO Char
receberEscolha estado = do
  putStrLn "-------------------------------------------------"
  putStrLn ""
  putStrLn $ "[ID " ++ [_id situacao] ++ "]. " ++ _descricao situacao
  putStrLn "Escolha uma opção (1 ou 2):"
  putStrLn ""
  putStrLn $ "1. " ++ _opcao1 situacao
  putStrLn $ "2. " ++ _opcao2 situacao
  putStrLn "Ou use seu dispositivo Zipper para voltar para um momento anterior na linha do tempo digitando Z"
  putStrLn ""
  lerDoTeclado "Digite sua escolha: "
  where situacao = getElemento (getFoco estado)

receberId :: ArvoreZipper Situacao -> IO Char
receberId _ = lerDoTeclado "Digite o identificador do momento anterior: (apenas 1 letra)"

voltarParaPontoJ :: Char -> ArvoreZipper Situacao -> ArvoreZipper Situacao
voltarParaPontoJ _ z@(Zipper Vazio _)                = z
voltarParaPontoJ _ z@(Zipper (No _ _ _)  (Passos [])) = z
voltarParaPontoJ y z@(Zipper (No _ x _) (Passos _))
  | _id x /= y = voltarParaPontoJ y (subir z)
  | otherwise = z

processarEntrada :: Char -> ArvoreZipper Situacao -> IO (Zipper Arvore Passos Situacao)
processarEntrada opcao arvoreZipper
  | opcao == '1' = return (irParaEsquerda arvoreZipper)
  | opcao == '2' = return (irParaDireita arvoreZipper)
  | opcao == 'Z' = do
        identificador <- receberId arvoreZipper
        return (voltarParaPontoJ identificador arvoreZipper)
  | otherwise = return arvoreZipper

passoParaSituacao :: Passo Situacao -> Situacao
passoParaSituacao (Esq s _) = s
passoParaSituacao (Dir s _) = s

imprimirOpcoes :: [Passo Situacao] -> IO ()
imprimirOpcoes passos = do
  let ids = map (_id . passoParaSituacao) passos
  putStrLn $ "Opções: " ++ intercalate ", " (map show ids)

processarEntradaFinal :: ArvoreZipper Situacao -> IO (Zipper Arvore Passos Situacao)
processarEntradaFinal arvoreZipper@(Zipper _ (Passos passos)) = do
        putStrLn "Você chegou a uma ponta sem saída. Escolha um momento anterior para retornar com seu TimeZipper."
        imprimirOpcoes passos
        identificador <- receberId arvoreZipper
        return (voltarParaPontoJ identificador arvoreZipper)

fimDeJogo :: ArvoreZipper Situacao -> IO (Bool, Maybe Resultado)
fimDeJogo (Zipper Vazio _) = return (False, Nothing)
fimDeJogo (Zipper (No _ estado _) _) =
  case _conteudoSituacao estado of
    Final resultado -> return (True, Just resultado)
    NaoFinal        -> return (False, Nothing)

imprimirEstado :: ArvoreZipper Situacao -> IO()
imprimirEstado (Zipper Vazio _) = putStrLn "Estado atual inválido."
imprimirEstado (Zipper (No _ estado _) _) = print estado

---------------Main---------------------------

iniciarJogo :: IO()
iniciarJogo = do
    putStrLn "-=- Bem vindo ao TimeZipper! -=-"
    putStrLn "Este é um jogo de decisões. Serão apresentadas situações a você, e você terá que escolher."
    putStrLn "TimeZipper se passa em um mundo diferente. Você é uma pessoa detetive, mas gosta da adrenalina da rua. Cada um dos detetives do seu departamento possui um TimeZipper, um dispositivo capaz de gravar decisões na linha temporal, e manipulá-la para retornar para o momento de cada uma dessas decisões. Seu uso é restrito, e ele consegue gravar apenas algumas decisões antes de ficar com sua memória cheia. Por isso, você o mantém desligado."
    putStrLn ""
    putStrLn "Você está à paisana na frente do banco Datomic. É um dia bem ensolarado, e você observa as pessoas andando do outro lado da rua. A brisa leve bate em seu rosto, e você tem uma intuição que algo está prestes a acontecer..."
    putStrLn ""
    putStrLn "E realmente acontece! As portas da frente do banco Datomic se abrem, e você ouve gritaria e confusão vindo de dentro do banco. Você institivamente ativa seu TimeZipper."
    let arvore = criarArvoreZipperDeLista [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18]
    lacoDoJogo arvore
    
main :: IO ()
main = do
    iniciarJogo