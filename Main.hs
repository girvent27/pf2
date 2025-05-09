module Main (main) where

import Tipos
import Funcoes
import FuncoesInterface
import Persistencia
import System.IO
import Control.Monad (unless)

main :: IO()
main = do hSetBuffering stdout NoBuffering
          listaPreExistenteUsuário <- carregarUsuárioDeArquivo "usuarios.txt"
          listaPreExistenteLivro <- carregarLivroDeArquivo "livros.txt"
          listaPreExistenteEmprestimo <- carregarEmprestimoDeArquivo "emprestimos.txt"
          mainLoop listaPreExistenteUsuário listaPreExistenteLivro listaPreExistenteEmprestimo

--A função mainLoop faz repetição da interface do usuário a ser imprimida.
--Ela torna possível a interação do usuário a partir do case
--Com base na opção, chama funções do módulo FuncoesInterface.hs        
mainLoop :: [Usuário] -> [Livro] -> [Emprestimos] -> IO ()
mainLoop listaUsuário listaLivro listaEmprestimo = do
   listaUsuário <- carregarUsuárioDeArquivo "usuarios.txt"
   listaLivro <- carregarLivroDeArquivo "livros.txt"
   listaEmprestimo <- carregarEmprestimoDeArquivo "emprestimos.txt"
   mostrarCabecalho
   menuPrincipal
   opcao <- getLine
   do
     case opcao of
          "1"  ->  do
                   novoLivro <- adicionarLivroIO listaLivro
                   mainLoop listaUsuário novoLivro listaEmprestimo
          "2"  ->  do
                   novoUsuário <- adicionarUsuárioIO listaUsuário
                   mainLoop novoUsuário listaLivro listaEmprestimo
          -- chama o novo menu para os empréstimos
          "3"  ->  do
                   (novoUsuário, novoLivro, novoEmprestimo) <- menuEmprestimosIO listaUsuário listaLivro listaEmprestimo
                   mainLoop novoUsuário novoLivro novoEmprestimo
          --chama o novo menu para relatórios
          "4"  ->  do 
                   menuRelatóriosIO listaUsuário listaLivro listaEmprestimo
                   mainLoop listaUsuário listaLivro listaEmprestimo
          --chama o novo menu de edição dos livros
          "5"  ->  do
                   novoLivro <- editarLivroIO listaLivro
                   mainLoop listaUsuário novoLivro listaEmprestimo
          --chama o novo menu de edição dos usuários
          "6"  ->  do
                   novoUsuário <- editarUsuárioIO listaUsuário
                   mainLoop novoUsuário listaLivro listaEmprestimo
          "7"  ->  salvaBibliotecaIO listaUsuário listaLivro listaEmprestimo
          _    ->  do 
                   putStrLn "Opção inválida, tente novamente"
                   mainLoop listaUsuário listaLivro listaEmprestimo
          
          
mostrarCabecalho :: IO()
mostrarCabecalho = do
          putStrLn "=========================================="
          putStrLn "       SISTEMA DE GESTÃO - BIBLIOTECA     "
          putStrLn "=========================================="
          
         
menuPrincipal :: IO()
menuPrincipal = do
        putStrLn "╔════════════════════════════════════════╗"
        putStrLn "║         MENU PRINCIPAL                 ║"
        putStrLn "╠════════════════════════════════════════╣"
        putStrLn "║ 1  │ Cadastrar livros                  ║"
        putStrLn "║ 2  │ Cadastrar usuários                ║"
        putStrLn "║ 3  │ Empréstimo e Devolução            ║"
        putStrLn "║ 4  │ Relatórios                        ║"
        putStrLn "║ 5  │ Editar livro                      ║"
        putStrLn "║ 6  │ Editar Usuário                    ║"
        putStrLn "║ 7  │ Salvar e Sair                     ║"
        putStrLn "╚════════════════════════════════════════╝"
        putStrLn "Digite uma opção: "
         
