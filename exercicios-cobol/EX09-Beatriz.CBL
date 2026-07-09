      ******************************************************************
      * Author: Beatriz Lima
      * Date: 31/10
      * Purpose: Programa que armazenA em uma matriz, a partir
      *          da entrada de um número, informe os próximos 5 números
      *          pares em ordem crescente numa lista e depois os 5 números
      *          ímpares em ordem descrecente, e, ao final, perguntando se
      *          o usuário quer novamente inserir um novo número ou encerrar
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG09.
       AUTHOR. BEATRIZ.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 NUMERO-INICIAL  PIC 9(5).
       77 INDICE          PIC 9(2) VALUE 1.
       77 CONTINUAR       PIC X(1) VALUE 'S'.

       01 PARES.
           05 PAR PIC 9(5) OCCURS 5 TIMES.

       01 IMPARES.
           05 IMPAR PIC 9(5) OCCURS 5 TIMES.

       PROCEDURE DIVISION.
            PERFORM UNTIL CONTINUAR = 'N' OR CONTINUAR = 'n'
               DISPLAY 'DIGITE UM NUMERO INTEIRO QUALQUER: '
               ACCEPT NUMERO-INICIAL

               IF FUNCTION MOD(NUMERO-INICIAL, 2) = 0
                   ADD 2 TO NUMERO-INICIAL
               ELSE
                   ADD 1 TO NUMERO-INICIAL
               END-IF

               PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 5
                   MOVE NUMERO-INICIAL TO PAR(INDICE)
                   ADD 2 TO NUMERO-INICIAL
               END-PERFORM

               SUBTRACT 3 FROM NUMERO-INICIAL

               PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 5
                   MOVE NUMERO-INICIAL TO IMPAR(INDICE)
                   SUBTRACT 2 FROM NUMERO-INICIAL
               END-PERFORM

               DISPLAY ' '
               PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 5
                   DISPLAY 'PROXIMOS 5 NUMEROS PARES EM ORDEM CRESC: '
                   DISPLAY PAR(INDICE)
               END-PERFORM

               DISPLAY ' '
               PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 5
                   DISPLAY 'PROXIMOS 5 NUMEROS IMPARES EM ORDEM DECRE: '
                   DISPLAY IMPAR(INDICE)
               END-PERFORM

               DISPLAY 'DESEJA CONTINUAR (S/N)? '
               ACCEPT CONTINUAR
              END-PERFORM.
            STOP RUN.
       END PROGRAM PROG09.
