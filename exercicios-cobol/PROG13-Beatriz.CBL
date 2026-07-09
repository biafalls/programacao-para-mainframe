      ******************************************************************
      * Author: Beatriz Lima
      * Date: 14/11/2025
      * Purpose: O sistema da pizzaria registra pedidos de pizza, guarda
      *         os valores em uma tabela interna e permite visualizar todos
      *         os pedidos feitos. Ele também calcula automaticamente o total
      *         arrecadado e a média dos pedidos. Tudo funciona por meio de um
      *         menu simples, onde o usuário escolhe registrar, listar,
      *          calcular ou sair.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG13.
       AUTHOR. BEATRIZ LIMA.
      *=================================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *=================================================================
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OPCAO-MENU                     PIC 9.

       01  LIMITE-MAX-PEDIDOS             PIC 99 VALUE 50.

       01  QUANTIDADE-PEDIDOS             PIC 99 VALUE 0.

       01  INDICE-PEDIDO                  PIC 99.

       01  TABELA-PEDIDOS.
           05  VALOR-PEDIDO OCCURS 50 TIMES PIC 9(5)V99 VALUE 0.

       01  VALOR-DA-PIZZA                 PIC 9(5)V99.
       01  TOTAL-GERAL                    PIC 9(7)V99 VALUE 0.
       01  MEDIA-GERAL                    PIC 9(7)V99 VALUE 0.
      *=================================================================
       PROCEDURE DIVISION.
       INICIO.
           PERFORM MENU-PRINCIPAL
               UNTIL OPCAO-MENU = 4.

           STOP RUN.

       MENU-PRINCIPAL.
           DISPLAY " "
           DISPLAY "=============================================="
           DISPLAY "        SISTEMA DE PIZZARIA - MENU"
           DISPLAY "=============================================="
           DISPLAY "1 - Registrar novos pedidos de pizza"
           DISPLAY "2 - Exibir pedidos realizados"
           DISPLAY "3 - Calcular total e media dos pedidos"
           DISPLAY "4 - Sair"
           DISPLAY "Escolha uma opcao: "
           ACCEPT OPCAO-MENU

           EVALUATE OPCAO-MENU
               WHEN 1
                   DISPLAY " "
                   PERFORM REGISTRAR-PEDIDOS
               WHEN 2
                   DISPLAY " "
                   PERFORM EXIBIR-PEDIDOS
               WHEN 3
                   DISPLAY " "
                   PERFORM CALCULAR-VALORES
               WHEN 4
                   DISPLAY " "
                   DISPLAY "Encerrando o sistema da pizzaria..."
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY "Opcao invalida! Tente novamente."
           END-EVALUATE.

       REGISTRAR-PEDIDOS.
           DISPLAY "Quantos pedidos deseja registrar? (Max: 50)"
           ACCEPT QUANTIDADE-PEDIDOS

           IF QUANTIDADE-PEDIDOS > LIMITE-MAX-PEDIDOS
               DISPLAY "Erro: quantidade maior que o limite permitido!"
               MOVE 0 TO QUANTIDADE-PEDIDOS
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO INDICE-PEDIDO

           PERFORM UNTIL INDICE-PEDIDO > QUANTIDADE-PEDIDOS
               DISPLAY "Digite o valor da pizza do pedido "
                       INDICE-PEDIDO ": R$"
               ACCEPT VALOR-DA-PIZZA

               MOVE VALOR-DA-PIZZA TO VALOR-PEDIDO(INDICE-PEDIDO)

               ADD 1 TO INDICE-PEDIDO
           END-PERFORM.

           DISPLAY "Pedidos registrados com sucesso!".

       EXIBIR-PEDIDOS.
           IF QUANTIDADE-PEDIDOS = 0
               DISPLAY "Nenhum pedido registrado ate o momento."
               EXIT PARAGRAPH
           END-IF

           DISPLAY "========== LISTA DE PEDIDOS DA PIZZARIA =========="

           MOVE 1 TO INDICE-PEDIDO
           PERFORM UNTIL INDICE-PEDIDO > QUANTIDADE-PEDIDOS
               DISPLAY "Pedido " INDICE-PEDIDO ": R$ "
                       VALOR-PEDIDO(INDICE-PEDIDO)
               ADD 1 TO INDICE-PEDIDO
           END-PERFORM.

           DISPLAY "==================================================".

       CALCULAR-VALORES.
           IF QUANTIDADE-PEDIDOS = 0
            DISPLAY "Nenhum pedido registrado! Nao ha calculos a fazer."
            EXIT PARAGRAPH
           END-IF

           MOVE 0 TO TOTAL-GERAL

           MOVE 1 TO INDICE-PEDIDO
           PERFORM UNTIL INDICE-PEDIDO > QUANTIDADE-PEDIDOS
               ADD VALOR-PEDIDO(INDICE-PEDIDO) TO TOTAL-GERAL
               ADD 1 TO INDICE-PEDIDO
           END-PERFORM.

           COMPUTE MEDIA-GERAL =
                   TOTAL-GERAL / QUANTIDADE-PEDIDOS.

           DISPLAY "TOTAL ARRECADADO: R$ " TOTAL-GERAL
           DISPLAY "MEDIA POR PEDIDO: R$ " MEDIA-GERAL.

       END PROGRAM PROG13.
