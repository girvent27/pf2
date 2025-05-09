module Funcoes where

import Tipos
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Data.List (find)

-----------------------------------
--------     FUNÇÕES     ----------
-----------------------------------

-- Insere usuário no início da lista 
adicionarUsuário :: Usuário -> [Usuário] -> [Usuário]
adicionarUsuário novoUsuário usuário = novoUsuário : usuário

-- Insere livro no início da lista 
adicionarLivro :: Livro -> [Livro] -> [Livro]
adicionarLivro novoLivro livro = novoLivro : livro

-- Insere empréstimo no início da lista 
adicionarEmpréstimo :: Emprestimos -> [Emprestimos] -> [Emprestimos]
adicionarEmpréstimo novoEmpréstimo empréstimo = novoEmpréstimo : empréstimo

-- Remove usuário com matrícula correspondente
removerUsuário :: Int -> [Usuário] -> [Usuário]
removerUsuário _ [] = []
removerUsuário id (x:xs)| matrícula x == id = xs
                        | otherwise = x : removerUsuário id xs
                        
-- Remove livro com código correspondente                     
removerLivro :: Int -> [Livro] -> [Livro]
removerLivro _ [] = []
removerLivro id (x:xs)  | código x == id = xs
                        | otherwise = x : removerLivro id xs

-- Remove empréstimo com código correspondente                     
removerEmpréstimo :: Int -> [Emprestimos] -> [Emprestimos]
removerEmpréstimo _ [] = []
removerEmpréstimo id (x:xs)  | identificador x == id = xs 
                             | otherwise = x : removerEmpréstimo id xs
                        
--Altera o ano de um livro                    
editarLivroAno :: Int -> Int-> [Livro] -> [Livro]
editarLivroAno _ a [] = []
editarLivroAno id a (x:xs) |código x == id = adicionarLivro (Livro (código x) (título x) (autor x) a (livre x) [])  xs
                           |otherwise = x : editarLivroAno id a xs
                       
--Altera o autor de um livro                    
editarLivroAutor :: Int -> String-> [Livro] -> [Livro]
editarLivroAutor _ a [] = []
editarLivroAutor id a (x:xs) | código x == id = adicionarLivro (Livro (código x) (título x) a (ano x) (livre x) [])  xs
                             |otherwise = x : editarLivroAutor id a xs
                             
--Altera o título de um livro                    
editarLivroTítulo :: Int -> String-> [Livro] -> [Livro]
editarLivroTítulo _ a [] = []
editarLivroTítulo id a (x:xs) | código x == id = adicionarLivro (Livro (código x) a (autor x) (ano x) (livre x) [])  xs
                              |otherwise = x : editarLivroTítulo id a xs

--Altera o status de livre quando um livro é emprestado ou devolvido                    
editarLivroStatus :: Int -> [Livro] -> [Livro]
editarLivroStatus _  [] = []
editarLivroStatus id (x:xs)   |código x == id && livre x = adicionarLivro (Livro (código x) (título x) (autor x) (ano x) False [])  xs
                              |código x == id = adicionarLivro (Livro (código x) (título x) (autor x) (ano x) True [])  xs
                              |otherwise = x : editarLivroStatus id xs
                          
--Altera o nome de um usuário                    
editarUsuárioNome :: Int -> String-> [Usuário] -> [Usuário]
editarUsuárioNome _ a [] = []
editarUsuárioNome id a (x:xs) |matrícula x == id = adicionarUsuário (Usuário (matrícula x) a (email x) (histórico x))  xs
                              |otherwise = x : editarUsuárioNome id a xs
                       
--Altera o email de um usuário                    
editarUsuárioEmail :: Int -> String-> [Usuário] -> [Usuário]
editarUsuárioEmail _ a [] = []
editarUsuárioEmail id a (x:xs) | matrícula x == id = adicionarUsuário (Usuário (matrícula x) (nome x) a (histórico x))  xs
                               |otherwise = x : editarUsuárioEmail id a xs     
                            
--Salva o histórico de empréstimos do usuário
históricoUsuário :: Int -> Int -> [Usuário]-> [Usuário]
históricoUsuário _ _ [] = []
históricoUsuário codEmprestimo id (x:xs) | matrícula x == id = adicionarUsuário (Usuário (matrícula x) (nome x) (email x) (histórico x ++ [codEmprestimo])) xs
                                         |otherwise = x : históricoUsuário codEmprestimo id xs
                             

--Cria uma lista com livros disponíveis
listaLivrosDisponíveis :: [Livro] -> [Livro]
listaLivrosDisponíveis [] = []
listaLivrosDisponíveis (x:xs) | livre x = x : listaLivrosDisponíveis xs
                               | otherwise = listaLivrosDisponíveis xs
                               
--Confere se um livro está disponíveis
livroDisponível :: Int -> [Livro]-> Bool
livroDisponível _ [] = False
livroDisponível id (x:xs) | (código x) == id && livre x = True
                          | (código x) == id && not (livre x) = False
                          | otherwise = livroDisponível id xs

--Cria uma lista com livros indisponíveis
listaLivrosIndisponiveis :: [Livro] -> [Livro]
listaLivrosIndisponiveis [] = []
listaLivrosIndisponiveis (x:xs) | livre x = listaLivrosIndisponiveis xs
                                | otherwise = x : listaLivrosIndisponiveis xs

--Cria uma lista com livros que apresentam lista de espera não vazia
listaLivrosComReserva :: [Livro] -> [Livro]
listaLivrosComReserva [] = []
listaLivrosComReserva (x:xs)   | (listaEspera x) == [] = listaLivrosComReserva xs
                               | otherwise = x : listaLivrosComReserva xs
                              
--Adiciona um nome na lista de espera
lEspera :: Int -> Int -> [Livro]-> [Livro]
lEspera _ _ [] = []
lEspera usuário id (x:xs) |código x == id = adicionarLivro (Livro (código x) (título x) (autor x) (ano x) (livre x) (listaEspera x ++ [usuário])) xs
                          |otherwise = x : lEspera usuário id xs

--Buscar um empréstimo a partir do código do livro
buscaEmp :: Int -> [Emprestimos] -> Int
buscaEmp _ [] = -1
buscaEmp código (x:xs) |(livro x) == código = (identificador x)
                       |otherwise = buscaEmp código xs
                       
--Buscar uma matrícula na lista de espera de um livro
buscaEspera :: Int -> Int -> [Livro] -> Bool
buscaEspera _ _ [] = False
buscaEspera cod id (x:xs) |(código x)==id && (buscaAux cod (listaEspera x)) = True
                          |(código x)==id = False
                          |otherwise = buscaEspera cod id xs
                          
--Retorna a lista de espera de um determinado livro
nomesListaEsp :: Int -> [Livro]->[Int]
nomesListaEsp _ [] = []
nomesListaEsp id (x:xs)  |(código x)==id = listaEspera x
                         |otherwise = nomesListaEsp id xs
                         
--Retorna a histórico do usuário
histUsuário :: Int->[Usuário]->[Livro]->[String]
histUsuário _ [] _ = [[]]
histUsuário id (x:xs) ys |(matrícula x) == id = nomeReserva (histórico x) ys
                         |otherwise = histUsuário id xs ys
                          
--Retorna nome dos usuários na lista de espera de um livro
nomesLista::[Int]->[Usuário]->[String]
nomesLista [] _ = []
nomesLista (x:xs) (y:ys) |(matrícula y) == x = nomeUsuário x (y:ys) : nomesLista xs ys
                         |otherwise = nomesLista xs (y:ys)


adicionaBiblioteca :: [Livro] -> [Emprestimos] -> [Usuário] -> [Biblioteca] -> [Biblioteca]
adicionaBiblioteca livros emprestimos usuarios biblioteca = biblioteca ++ [Biblioteca usuarios livros emprestimos]

                      
-----------------------------------
------- FUNÇÕES ADICIONAIS --------
-----------------------------------

-- Achar maior Código
acharMaiorCódigo :: [Livro] -> Int
acharMaiorCódigo [] = 0
acharMaiorCódigo [x] = código x
acharMaiorCódigo (x:y:xs) | código x > código y = max (código x) (acharMaiorCódigo (x:xs))
                          | otherwise = max (código y) (acharMaiorCódigo (y:xs))
                          

-- Achar maior Matrículahistórico
acharMaiorMatrícula :: [Usuário] -> Int
acharMaiorMatrícula [] = 0
acharMaiorMatrícula [x] = matrícula x
acharMaiorMatrícula (x:y:xs) | matrícula x > matrícula y = max (matrícula x) (acharMaiorMatrícula (x:xs))
                             | otherwise = max (matrícula y) (acharMaiorMatrícula (y:xs))
                            
-- Achar maior Identificador de Empréstimo
acharMaiorID :: [Emprestimos] -> Int
acharMaiorID [] = 0
acharMaiorID [x] = identificador x
acharMaiorID (x:y:xs) | identificador x > identificador y = max (identificador x) (acharMaiorID (x:xs))
                      | otherwise = max (identificador y) (acharMaiorID (y:xs))
   
--Buscar usuário na lista de espera de um livro 
buscaAux :: Int -> [Int] -> Bool
buscaAux _ [] = False
buscaAux mat (x:xs) |x==mat = True
                    |otherwise = buscaAux mat xs

--Retorna o nome do livro pelo código
nomeLivro :: Int->[Livro]->String
nomeLivro _ [] = "0"
nomeLivro id (x:xs) |(código x)==id= (título x)
                    |otherwise = nomeLivro id xs
                    
--Retorna o nome de uma pessoa pela matrícula
nomeUsuário :: Int -> [Usuário]->String
nomeUsuário _ [] = "0"
nomeUsuário id (x:xs) |(matrícula x)==id= (nome x)
                      |otherwise = nomeUsuário id xs
                      
--Retorna o  nome de um livro da reserva
nomeReserva :: [Int] -> [Livro] -> [String]
nomeReserva [] _ = []
nomeReserva (x:xs) livros =
    case find (\livro -> código livro == x) livros of
        Just livro -> título livro : nomeReserva xs livros
        Nothing -> nomeReserva xs livros
