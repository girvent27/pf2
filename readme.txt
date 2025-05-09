_______________________________________________________________
====== Sistema de Gerenciamento de Biblioteca em Haskell ======
_______________________________________________________________
--Versão do Haskell: v8.8.4 (ou acima)


+ Compilação       
   Pelo terminal, qualquer sistema com Haskell já instalado, vá 
 ao diretório do arquivo do gerenciador  e digite os seguintes
 comandos:

- Se o Main.exe ou Main não estiver compilado:

      ghc --make Main.hs
      
- Depois digite o comando:

      .\Main

+ Uso
   Programa  para cadastrar livros e usuários, gerenciar empréstimos e
 devoluções com opções para: inserção, remoção, e gerar relatórios.
   Os livros são organizadas com as seguintes informações:
  
   - Cadastros:
      .Livros: código, título, autor, ano, status (disponível/indisponível).
      .Usuários: matrícula, nome, e-mail, histórico de empréstimos.

   - Operações:
      .Empréstimos e devoluções (atualiza status automaticamente).
      .Lista de espera para livros indisponíveis.
      .Edição de dados (livros e usuários).

   - Relatórios:
      .Listar livros disponíveis/indisponíveis.
      .Histórico de empréstimos por usuário.
      .Livros com reservas ativas.

   - Persistência:
      .Dados salvos automaticamente em usuarios.txt, livros.txt e emprestimos.txt.

+ Menus Interativos
   Navegue pelas opções numéricas para:
   - Cadastrar/editar livros e usuários.
   - Gerenciar empréstimos e devoluções.
   - Gerar relatórios detalhados.
   - Salvar e sair do sistema.

+ Estrutura do Código
   - Módulos especializados: Tipos.hs, Funcoes.hs, FuncoesInterface.hs, Persistencia.hs.
   - Funções puras para manipulação de dados.
   - Interface intuitiva com feedback visual (✔ para sucesso, mensagens de erro).