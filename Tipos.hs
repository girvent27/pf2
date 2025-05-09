module Tipos where

data Usuário = Usuário { 
matrícula   :: Int
, nome      :: String
, email     :: String
,histórico   :: [Int]} deriving (Show, Read, Eq)

data Livro = Livro { 
código        :: Int
,título       :: String
, autor       :: String
, ano         :: Int
,livre        :: Bool
, listaEspera :: [Int]} deriving (Show, Read, Eq)

data Emprestimos = Emprestimos {
identificador   ::Int
,livro          :: Int
, usuário       :: Int} deriving (Show, Read, Eq) 

data Biblioteca = Biblioteca { 
usuários      :: [Usuário]
, livros      :: [Livro]
, emprestimos :: [Emprestimos]} deriving (Show, Read, Eq)
