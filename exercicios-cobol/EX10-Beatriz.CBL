      ******************************************************************
      * Author: BEATRIZ LIMA
      * Date: 31/10/2025
      * Purpose: Programa que armazena em uma matriz, a partir da entrada
      *          de um número, informe os próximos 5 números primos, em ordem
      *          crescente e depois em ordem decrescente e, ao final,
      *          perguntando se o usuário quer novamente inserir um novo
      *          número ou encerrar
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG10.
       AUTHOR. BEATRIZ.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 NUMERO-INICIAL     PIC 9(5).
       77 NUMERO-TESTE       PIC 9(5).
       77 DIVISOR            PIC 9(5).
       77 RESTO              PIC 9(5).
       77 METADE             PIC 9(5).
       77 CONTADOR-PRIMOS    PIC 9(1) VALUE 0.
       77 E-PRIMO            PIC X(3) VALUE 'NAO'.
       77 CONTINUAR          PIC X(1) VALUE 'S'.

       01 PRIMOS.
           05 PRIMO       PIC 9(5) OCCURS 5 TIMES.

       PROCEDURE DIVISION.
           PERFORM UNTIL CONTINUAR = 'N' OR CONTINUAR = 'n'
             DISPLAY 'DIGITE UM NUMERO INTEIRO QUALQUER: '
             ACCEPT NUMERO-INICIAL

             ADD 1 TO NUMERO-INICIAL
             MOVE 0 TO CONTADOR-PRIMOS

             PERFORM UNTIL CONTADOR-PRIMOS = 5
               MOVE 'SIM' TO E-PRIMO
               MOVE 2 TO DIVISOR
               COMPUTE METADE = NUMERO-INICIAL / 2

               IF NUMERO-INICIAL > 2
                  PERFORM UNTIL DIVISOR > METADE OR E-PRIMO = 'NAO'
                    COMPUTE RESTO = FUNCTION MOD(NUMERO-INICIAL,DIVISOR)
                    IF RESTO = 10
                       MOVE 'NAO' TO E-PRIMO
                    ELSE
                       ADD 1 TO DIVISOR
                    END-IF
               END-PERFORM
               END-IF

               IF E-PRIMO = 'SIM'
                   ADD 1 TO CONTADOR-PRIMOS
                   MOVE NUMERO-INICIAL TO PRIMO(CONTADOR-PRIMOS)
               END-IF

               ADD 1 TO NUMERO-INICIAL
           END-PERFORM

           DISPLAY ' '
           DISPLAY 'PROXIMOS 5 NUMEROS PRIMOS EM ORDEM CRESCENTE: '
             PERFORM VARYING DIVISOR FROM 1 BY 1 UNTIL DIVISOR > 5
               DISPLAY PRIMO(DIVISOR)
             END-PERFORM

               DISPLAY ' '
           DISPLAY 'PROXIMOS 5 NUMEROS PRIMOS EM ORDEM DECRESCENTE: '
             PERFORM VARYING DIVISOR FROM 5 BY -1 UNTIL DIVISOR < 1
               DISPLAY PRIMO(DIVISOR)
             END-PERFORM

           DISPLAY 'DESEJA CONTINUAR (S/N)? '
           ACCEPT CONTINUAR
           END-PERFORM.

           STOP RUN.
       END PROGRAM PROG10.
