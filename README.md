# Processador de CRM

Este é um script em Clojure para processar planilhas de CRM. Ele pode detectar automaticamente as colunas de nome, telefone e carro, ou permitir que o usuário especifique as colunas manualmente.

## Pré-requisitos

- Java Development Kit (JDK)
- Leiningen

## Como usar

1. **Clone o repositório:**

   ```bash
   git clone <url-do-repositorio>
   cd <nome-do-repositorio>
   ```

2. **Prepare o arquivo de entrada:**

   - Coloque sua planilha na raiz do projeto com o nome `base_teste.xlsx`.
   - A planilha não deve ter uma linha de cabeçalho.

3. **Instale as dependências:**

   ```bash
   ./lein deps
   ```

4. **Execute o projeto:**

   ```bash
   ./lein run
   ```

5. **Siga as instruções no terminal:**

   - O programa irá perguntar se você quer usar o modo automático ou manual.
   - **Modo Automático:** O programa tentará adivinhar as colunas corretas.
   - **Modo Manual:** O programa pedirá que você insira o índice de cada coluna (começando em 0).

6. **Verifique o arquivo de saída:**

   - Após a execução, um arquivo chamado `saida_para_crm_inferida.csv` será criado na raiz do projeto com os dados processados.
