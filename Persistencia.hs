module Persistencia where

import Funcoes
import Tipos
import System.IO
import Control.Exception (catch, IOException)

-- Função para salvar usuários em arquivos após serializar a informação
salvarUsuárioEmArquivo :: FilePath -> [Usuário] -> IO()
salvarUsuárioEmArquivo path usuário = do
    withFile path WriteMode (\h -> hPutStr h (show usuário))

-- Função para carregar usuários de um arquivo
carregarUsuárioDeArquivo :: FilePath -> IO [Usuário]
carregarUsuárioDeArquivo path = do
    conteudo <- readFile path `catch` handler
    if null conteudo 
       then return []
       else case reads conteudo of
            [(usuário, "")] -> return usuário
            _ -> return []
  where
    handler :: IOException -> IO String
    handler _ = return "[]"
    
-- Função para salvar livros em arquivos após serializar a informação
salvarLivroEmArquivo :: FilePath -> [Livro] -> IO()
salvarLivroEmArquivo path livro = do
    withFile path WriteMode (\h -> hPutStr h (show livro))

-- Função para carregar livros de um arquivo
carregarLivroDeArquivo :: FilePath -> IO [Livro]
carregarLivroDeArquivo path = do
    conteudo <- readFile path `catch` handler
    if null conteudo 
       then return []
       else case reads conteudo of
            [(livro, "")] -> return livro
            _ -> return []
  where
    handler :: IOException -> IO String
    handler _ = return "[]"
    
-- Função para salvar empréstimos em arquivos após serializar a informação
salvarEmprestimoEmArquivo :: FilePath -> [Emprestimos] -> IO()
salvarEmprestimoEmArquivo path emprestimos = do
    withFile path WriteMode (\h -> hPutStr h (show emprestimos))

-- Função para carregar empréstimos de um arquivo
carregarEmprestimoDeArquivo :: FilePath -> IO [Emprestimos]
carregarEmprestimoDeArquivo path = do
    conteudo <- readFile path `catch` handler
    if null conteudo 
       then return []
       else case reads conteudo of
            [(emprestimos, "")] -> return emprestimos
            _ -> return []
  where
    handler :: IOException -> IO String
    handler _ = return "[]"
    
--Função para salvar Biblioteca (junção de usuarios, emprestimos e livros) após serializar informação
salvarBibliotecaEmArquivo :: FilePath -> [Biblioteca] -> IO()
salvarBibliotecaEmArquivo path biblioteca = do
    withFile path WriteMode (\h -> hPutStr h (show biblioteca))

-- Função para carregar Biblioteca de um arquivo
carregarBibliotecaDeArquivo :: FilePath -> IO [Biblioteca]
carregarBibliotecaDeArquivo path = do
    conteudo <- readFile path `catch` handler
    if null conteudo 
       then return []
       else case reads conteudo of
            [(biblioteca, "")] -> return biblioteca
            _ -> return []
  where
    handler :: IOException -> IO String
    handler _ = return "[]"

