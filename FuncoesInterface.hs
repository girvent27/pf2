module FuncoesInterface where

import Tipos
import Funcoes
import Persistencia
import Data.Char
import Data.Time.Calendar
import Data.Time (getCurrentTime, utctDay, Day)
import Data.List (find, intercalate)
import Text.Read (readMaybe)
import Text.Printf (printf)

-------------------------------------------
----------- TRATAMENTO DE ERROS -----------
-------------------------------------------

-- Função para ler um Int com tratamento de erro
lerInteiro :: String -> IO Int
lerInteiro prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just n -> return n
        Nothing -> do
            putStrLn "Entrada inválida. Por favor, digite um número inteiro."
            lerInteiro prompt

-------------------------------------------
------- FUNÇÕES DE ENTRADA E SAÍDA --------
-------------------------------------------
adicionarLivroIO :: [Livro] -> IO [Livro]
adicionarLivroIO l = do
          putStrLn "Título:"
          titulo <- getLine       
          putStrLn "Autor:"
          autor <- getLine
          ano <- lerInteiro "Ano:"

          let id1 = case l of
                    [] -> 1
                    _  -> acharMaiorCódigo l + 1
              ano1 = ano
              l1 = Livro id1 titulo autor ano1 True []
              listaAtual = adicionarLivro l1 l
              
          putStrLn "\n✔ Livro adicionado com sucesso!\n"   
          salvarLivroEmArquivo "livros.txt" listaAtual
          return listaAtual
       where
          parsePrazo "" = Nothing
          parsePrazo str = case reads str of
                           [(d, "")] -> Just d
                           _ -> Nothing
                           
          
adicionarUsuárioIO :: [Usuário] -> IO [Usuário]
adicionarUsuárioIO l = do
          putStrLn "Nome:"
          nome <- getLine       
          putStrLn "Email:"
          email <- getLine
                      
          let id1 = case l of
                    [] -> 1
                    _  -> acharMaiorMatrícula l + 1
              l1 = Usuário id1 nome email [] 
              listaAtual = adicionarUsuário l1 l
         
          putStrLn "\n✔ Usuário adicionado com sucesso!\n"   
          salvarUsuárioEmArquivo "usuarios.txt" listaAtual
          return listaAtual
       where
          parsePrazo "" = Nothing
          parsePrazo str = case reads str of
                           [(d, "")] -> Just d
                           _ -> Nothing
                           
adicionarEmprestimoIO :: [Usuário]->[Livro]->[Emprestimos]->IO ([Usuário],[Livro],[Emprestimos])
adicionarEmprestimoIO lU ll lE = do
              códigoLivro1 <- lerInteiro "Digite o código do livro:"
              matrículaUsuário1 <- lerInteiro "Digite sua matrícula:"
              
              let identificador1 = acharMaiorID lE + 1
                  listaAuxEmpr = Emprestimos identificador1 códigoLivro1 matrículaUsuário1
                  listaAuxHist = históricoUsuário códigoLivro1 matrículaUsuário1 lU
                  listaAuxLivr = editarLivroStatus códigoLivro1 ll
                  listaAuxEsper = lEspera matrículaUsuário1 códigoLivro1  ll
                  
                  verifica = livroDisponível códigoLivro1 ll 
                  buscador = buscaEspera matrículaUsuário1 códigoLivro1  ll

                  listaEmprestimo = if verifica then adicionarEmpréstimo listaAuxEmpr lE else lE
                  listaUsuário = if verifica then listaAuxHist else lU
                  listaLivro |verifica = listaAuxLivr 
                             |buscador = ll
                             |otherwise = listaAuxEsper
          
              salvarUsuárioEmArquivo "usuarios.txt" listaUsuário
              salvarEmprestimoEmArquivo "emprestimos.txt" listaEmprestimo
              salvarLivroEmArquivo "livros.txt" listaLivro
              return (listaUsuário, listaLivro, listaEmprestimo)

                           
removerEmprestimoIO :: [Livro]->[Emprestimos]->IO ([Livro],[Emprestimos])
removerEmprestimoIO ll lE = do
              códigoLivro1 <- lerInteiro "Digite o código do livro:"
              let identificador1 = buscaEmp códigoLivro1 lE
                  
              if (identificador1 == -1) then do
                  putStrLn "Código não encontrado"
                  return (ll,lE)
              else do
                let listaLivro = editarLivroStatus códigoLivro1 ll
                    listaEmprestimo = removerEmpréstimo identificador1 lE
                salvarEmprestimoEmArquivo "emprestimos.txt" listaEmprestimo
                salvarLivroEmArquivo "livros.txt" listaLivro
                putStrLn "Devolução realizada com sucesso"
                return (listaLivro, listaEmprestimo)

imprimirLivroIO :: [Livro] -> IO ()
imprimirLivroIO livro = do
                      let livrosDisponiveis = listaLivrosDisponíveis livro
                          livrosIndisp = listaLivrosIndisponiveis livro
                      if null livrosDisponiveis 
                              then putStrLn "Nenhum livro disponível"
                              else mapM_ mostrarDisponiveis livrosDisponiveis
                      if null livrosIndisp 
                              then putStrLn "Nenhum livro indisponível"
                              else mapM_ mostrarIndisp livrosIndisp
                      where 
                        mostrarDisponiveis livro = do
                              putStrLn $ "Livro disponível:" ++
                                         "Código: "     ++ show (código livro) ++
                                         " | Título: "  ++ show (título livro) ++
                                         " | Autor: "   ++ show (autor livro) ++
                                         " | Ano: "     ++ show (ano livro)
                        mostrarIndisp livros = do 
                              putStrLn $ "Livro indisponível:" ++
                                         "Código: "       ++ show (código livros) ++
                                         " | Título: "    ++ show (título livros) ++
                                         " | Autor: "     ++ show (autor livros) ++
                                         " | Ano: "       ++ show (ano livros)
                                          
listarEmprestimoIO :: [Emprestimos] -> IO ()
listarEmprestimoIO emprestimo = do
                putStrLn "Histórico de emprestimos"         
                if null emprestimo
                   then putStrLn "Nenhum emprestimo registrado"                                      
                   else mapM_ imprimirEmprestimo emprestimo
                where 
                   imprimirEmprestimo e = putStrLn $ "ID do livro: "  ++ show (livro e) ++ "|Matrícula do usuário: " ++ show (usuário e)
                              
listadeEsperaIO :: [Livro] -> IO ()
listadeEsperaIO livro = do
                      códigoStr <- lerInteiro "Digite o código do livro que deseja encontrar:"
                      let código = códigoStr
                          listaEsp = nomesListaEsp código livro
                      if null listaEsp 
                              then putStrLn "Código não encontrado ou lista de espera vazia"
                              else mapM_ mostrarLista listaEsp           
                      where 
                        mostrarLista lista = do
                              putStrLn $ "Matrícula: "   ++ show lista
                              
históricoEmprestimoIO :: [Usuário] -> [Livro] ->IO ()
históricoEmprestimoIO usuario livro = do
                      matriculaStr <- lerInteiro "Informe a matrícula:"
                      let matricula1 = matriculaStr 
                          listaHistorico = histUsuário matricula1 usuario livro
                      if null listaHistorico 
                              then putStrLn "Histórico vazio"
                              else mapM_ mostrarLista listaHistorico
                      where 
                        mostrarLista lista = do
                              putStrLn $ "Livro: "  ++ show (lista) 
                 
esperaLivroIO :: [Usuário]->[Livro] -> IO ()
esperaLivroIO usuarios livros = do
                      let listaEsp = listaLivrosComReserva livros
                      if null listaEsp 
                              then putStrLn "Nenhum livro com lista de espera"
                              else mapM_ mostrarLista listaEsp
                      where 
                        mostrarLista livro = do
                              putStrLn $ "Livro: "    ++  título livro
                              let codigosMat = listaEspera livro
                                  nomes = map (\mat -> nomeUsuário mat usuarios) codigosMat
                              mapM_ (\nome -> putStrLn $ "- Nome: " ++ nome) nomes     
                                  

                      
              
menuEmprestimosIO::[Usuário]->[Livro]->[Emprestimos]->IO ([Usuário],[Livro],[Emprestimos])
menuEmprestimosIO listaUsuário listaLivro listaEmprestimo = do
        putStrLn "╔════════════════════════════════════════╗"
        putStrLn "║              EMPRÉSTIMO                ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 1  │ Novo Empréstimo                   ║"
        putStrLn "║ 2  │ Devolução                         ║"
        putStrLn "║ 3  │ Lista de livros                   ║"
        putStrLn "║ 4  │ Lista de espera                   ║"
        putStrLn "║ 5  │ Retornar                          ║"
        putStrLn "╚════════════════════════════════════════╝"
        putStrLn "Digite uma opção: "
        opcao <- getLine
        case opcao of
          "1" -> do
                 (novoUsuário, novoLivro, novoEmprestimo) <- adicionarEmprestimoIO listaUsuário listaLivro listaEmprestimo 
                 menuEmprestimosIO novoUsuário novoLivro novoEmprestimo
          "2" -> do
                 (novoLivro, novoEmprestimo) <- removerEmprestimoIO listaLivro listaEmprestimo
                 menuEmprestimosIO listaUsuário novoLivro novoEmprestimo
          "3" -> do
                 imprimirLivroIO listaLivro
                 menuEmprestimosIO listaUsuário listaLivro listaEmprestimo
          "4" -> do 
                 listadeEsperaIO listaLivro
                 menuEmprestimosIO listaUsuário listaLivro listaEmprestimo
          "5" -> return (listaUsuário, listaLivro, listaEmprestimo)
          _   -> do 
                 putStrLn "Opção inválida, tente novamente"
                 menuEmprestimosIO listaUsuário listaLivro listaEmprestimo
                 
menuRelatóriosIO::[Usuário]->[Livro]->[Emprestimos]->IO ([Usuário],[Livro],[Emprestimos])
menuRelatóriosIO listaUsuário listaLivro listaEmprestimo = do
        putStrLn "╔════════════════════════════════════════╗"
        putStrLn "║              RELATÓRIOS                ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 1  │ Listar Empréstimo                 ║"
        putStrLn "║ 2  │ Histórico de Empréstimo           ║"
        putStrLn "║ 3  │ Livro com lista de espera         ║"
        putStrLn "║ 4  │ Retornar                          ║"
        putStrLn "╚════════════════════════════════════════╝"
        putStrLn "Digite uma opção: "
        opcao <- getLine
        case opcao of
          "1" -> do
                 listarEmprestimoIO listaEmprestimo 
                 menuRelatóriosIO listaUsuário listaLivro listaEmprestimo
          "2" -> do
                 históricoEmprestimoIO listaUsuário listaLivro
                 menuRelatóriosIO listaUsuário listaLivro listaEmprestimo
          "3" -> do
                 esperaLivroIO listaUsuário listaLivro 
                 menuRelatóriosIO listaUsuário listaLivro listaEmprestimo
          "4" -> return (listaUsuário, listaLivro, listaEmprestimo)
          _   -> do 
                 putStrLn "Opção inválida, tente novamente"
                 menuRelatóriosIO listaUsuário listaLivro listaEmprestimo
                 
--Salva em um arquivo biblioteca.txt as lista
salvaBibliotecaIO :: [Usuário] -> [Livro] -> [Emprestimos] -> IO()
salvaBibliotecaIO usuarios livros emprestimos = do
                                          biblioteca <- carregarBibliotecaDeArquivo "biblioteca.txt"
                                          case biblioteca of
                                                        [] -> do
                                                               putStrLn"O arquivo biblioteca.txt não existe. Deseja criar um novo? (S/N)"
                                                        _ -> do
                                                               putStrLn"O arquivo biblioteca.txt já existe. Deseja sobreescrever? (S/N)"
                                          opcao <- getLine
                                          case toUpper (head opcao) of
                                                        'S' -> do
                                                               salvarBibliotecaEmArquivo "biblioteca.txt" (adicionaBiblioteca livros emprestimos usuarios [])
                                                               return()
                                                        'N' -> do return()
                                                        _ -> do salvaBibliotecaIO usuarios livros emprestimos
                                                        
editarUsuárioIO :: [Usuário]-> IO([Usuário])
editarUsuárioIO listaUsuário = do
        putStrLn "╔════════════════════════════════════════╗"
        putStrLn "║             EDITAR USUÁRIO             ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 1  │ Editar nome                       ║"
        putStrLn "║ 2  │ Editar email                      ║"
        putStrLn "║ 3  │ Retornar                          ║"
        putStrLn "╚════════════════════════════════════════╝"
        putStrLn "Digite uma opção: "
        opcao <- getLine
        case opcao of
          "1" -> do
                 novoUsuário <- editarNomeIO listaUsuário 
                 editarUsuárioIO novoUsuário 
          "2" -> do
                 novoUsuário <- editarEmailIO listaUsuário 
                 editarUsuárioIO novoUsuário 
          "3" -> return listaUsuário
          _   -> do 
                 putStrLn "Opção inválida, tente novamente"
                 editarUsuárioIO listaUsuário 
                 
editarNomeIO :: [Usuário]->IO([Usuário])
editarNomeIO listaUsuário = do
             id <- lerInteiro "Digite a matrícula do usuário:"
             putStrLn "Digite o nome corrigido:"
             nome <-getLine
             let id1 = id
                 novoUsuário = editarUsuárioNome id1 nome listaUsuário
             salvarUsuárioEmArquivo "usuarios.txt" novoUsuário
             return novoUsuário
             
editarEmailIO :: [Usuário]->IO([Usuário])
editarEmailIO listaUsuário = do
             id <- lerInteiro "Digite a matrícula do usuário:"
             putStrLn "Digite o email corrigido:"
             email <-getLine
             let id1 = id
                 novoUsuário = editarUsuárioEmail id1 email listaUsuário
             salvarUsuárioEmArquivo "usuarios.txt" novoUsuário
             return novoUsuário
             
editarLivroIO :: [Livro]-> IO([Livro])
editarLivroIO listaLivro = do
        listaLivro <- carregarLivroDeArquivo "livros.txt"
        putStrLn "╔════════════════════════════════════════╗"
        putStrLn "║             EDITAR LIVRO               ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 1  │ Editar autor                      ║"
        putStrLn "║ 2  │ Editar título                     ║"
        putStrLn "║ 3  │ Editar ano                        ║"
        putStrLn "║ 4  │ Retornar                          ║"
        putStrLn "╚════════════════════════════════════════╝"
        putStrLn "Digite uma opção: "
        opcao <- getLine
        case opcao of
          "1" -> do
                 novoLivro <- editarAutorIO listaLivro 
                 editarLivroIO novoLivro 
          "2" -> do
                 novoLivro <- editarTituloIO listaLivro 
                 editarLivroIO novoLivro 
          "3" -> do
                 novoLivro <- editarAnoIO listaLivro 
                 editarLivroIO novoLivro 
          "4" -> return listaLivro
          _   -> do 
                 putStrLn "Opção inválida, tente novamente"
                 editarLivroIO listaLivro 
                 
editarAutorIO :: [Livro]->IO([Livro])
editarAutorIO listaLivro = do
             id <- lerInteiro "Digite o código do livro:"
             putStrLn "Digite o autor corrigido:"
             autor <-getLine
             let id1 = id
                 novoLivro = editarLivroAutor id1 autor listaLivro
             salvarLivroEmArquivo "livros.txt" novoLivro
             return novoLivro
             
editarTituloIO :: [Livro]->IO([Livro])
editarTituloIO listaLivro = do
             id <- lerInteiro "Digite o código do livro:"
             putStrLn "Digite o título corrigido:"
             titulo <-getLine
             let id1 = id
                 novoLivro = editarLivroTítulo id1 titulo listaLivro
             salvarLivroEmArquivo "livros.txt" novoLivro
             return novoLivro
             
editarAnoIO :: [Livro]->IO([Livro])
editarAnoIO listaLivro = do
             id <- lerInteiro "Digite o código do livro:"
             ano <- lerInteiro "Digite o ano corrigido:"             
             let id1 = id
                 ano1 = ano
                 novoLivro = editarLivroAno id1 ano1 listaLivro
             salvarLivroEmArquivo "livros.txt" novoLivro
             return novoLivro
