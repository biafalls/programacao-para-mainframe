      ******************************************************************
      * Author: Beatriz Lima
      * Date: 07/11/2025
      * Purpose: Programa que realiza o cálculo de seno, cosseno etangente.
      *          O usuário fará a entrada de dados via teclado, você armazenará
      *          os valores em tabela interna, e o programa deve perguntar qual
      *          cálculo deve ser realizado (seno, cosseno ou tangente). No
      *          final, deve perguntar se o usuário quer continuar ou encerrar
      *          o programa
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG11.
       AUTHOR. BEATRIZ LIMA.
       DATE-WRITTEN. 07-11-2025.
      *=================================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *=================================================================
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 OPCAO-CALCULO           PIC X VALUE "0".
       77 ANGULO-GRAUS            COMP-2 VALUE 0.
       77 ANGULO-RAD              COMP-2 VALUE 0.
       77 PI-CONSTANTE            COMP-2 VALUE 3.14159265.
       77 RESPOSTA-USUARIO        PIC X VALUE "S".
       77 RESULTADO-SENO          COMP-2 VALUE 0.
       77 RESULTADO-COSSENO       COMP-2 VALUE 0.
       77 RESULTADO-TANGENTE      COMP-2 VALUE 0.
       77 TERMO                   COMP-2 VALUE 0.
       77 NUMERADOR               COMP-2 VALUE 0.
       77 DENOMINADOR             COMP-2 VALUE 0.
       77 POTENCIA                COMP-2 VALUE 0.
       77 FATORIAL                COMP-2 VALUE 1.
       77 SINAL                   COMP-2 VALUE 1.
       77 I                       PIC 9(02) VALUE 0.
       77 BASE                    COMP-2 VALUE 0.
       77 EXPOENTE                COMP-2 VALUE 0.
       77 RESULTADO               COMP-2 VALUE 0.
       77 NUMERO                  COMP-2 VALUE 0.

       01 TABELA-ENTRADAS.
          05 DADOS-TRIG OCCURS 10 TIMES.
             10 ANGULO-TAB      COMP-2.
             10 RESULTADO-TAB   COMP-2.

       77 INDICE-TABELA          PIC 9(02) VALUE 1.

       PROCEDURE DIVISION.
       INICIO-PROGRAMA.
           PERFORM UNTIL RESPOSTA-USUARIO = "N"
           OR RESPOSTA-USUARIO = "n"
               DISPLAY "--------------------------------------------"
               DISPLAY "Escolha o calculo desejado:"
               DISPLAY "1 - Seno"
               DISPLAY "2 - Cosseno"
               DISPLAY "3 - Tangente"
               ACCEPT OPCAO-CALCULO
               DISPLAY "Digite o angulo em graus:"
               ACCEPT ANGULO-GRAUS

               COMPUTE ANGULO-RAD = ANGULO-GRAUS * (PI-CONSTANTE / 180)

               MOVE ANGULO-RAD TO ANGULO-TAB(INDICE-TABELA)

               EVALUATE OPCAO-CALCULO
                   WHEN "1"
                     PERFORM CALCULO-SENO
                     DISPLAY "Seno(" ANGULO-GRAUS "°) = " RESULTADO-SENO
                     MOVE RESULTADO-SENO TO RESULTADO-TAB(INDICE-TABELA)
                   WHEN "2"
                     PERFORM CALCULO-COSSENO
                     DISPLAY "Cosseno(" ANGULO-GRAUS "°) = "
                      RESULTADO-COSSENO
                     MOVE RESULTADO-COSSENO TO
                     RESULTADO-TAB(INDICE-TABELA)
                   WHEN "3"
                     PERFORM CALCULO-SENO
                     PERFORM CALCULO-COSSENO
                     IF RESULTADO-COSSENO NOT = 0
                         COMPUTE RESULTADO-TANGENTE =
                             RESULTADO-SENO / RESULTADO-COSSENO
                         DISPLAY "Tangente(" ANGULO-GRAUS "°) = "
                             RESULTADO-TANGENTE
                         MOVE RESULTADO-TANGENTE
                             TO RESULTADO-TAB(INDICE-TABELA)
                     ELSE
                       DISPLAY "Tangente indefinida (divisao por zero)."
                     END-IF
                   WHEN OTHER
                     DISPLAY "Opcao invalida."
               END-EVALUATE

               DISPLAY "Deseja realizar outro calculo? (S/N): "
               ACCEPT RESPOSTA-USUARIO
               ADD 1 TO INDICE-TABELA
           END-PERFORM.

           DISPLAY "Programa encerrado."
           STOP RUN.

       CALCULO-SENO.
           MOVE 0 TO RESULTADO-SENO
           MOVE 1 TO SINAL
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > 6
               COMPUTE EXPOENTE = (2 * I + 1)
               MOVE ANGULO-RAD TO BASE
               PERFORM CALCULO-POTENCIA
               MOVE RESULTADO TO POTENCIA

               COMPUTE NUMERO = (2 * I + 1)
               PERFORM CALCULO-FATORIAL
               MOVE RESULTADO TO FATORIAL

               COMPUTE TERMO = SINAL * (POTENCIA / FATORIAL)
               ADD TERMO TO RESULTADO-SENO
               MULTIPLY -1 BY SINAL
           END-PERFORM.

       CALCULO-COSSENO.
           MOVE 0 TO RESULTADO-COSSENO
           MOVE 1 TO SINAL
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > 6
               COMPUTE EXPOENTE = (2 * I)
               MOVE ANGULO-RAD TO BASE
               PERFORM CALCULO-POTENCIA
               MOVE RESULTADO TO POTENCIA

               COMPUTE NUMERO = (2 * I)
               PERFORM CALCULO-FATORIAL
               MOVE RESULTADO TO FATORIAL

               COMPUTE TERMO = SINAL * (POTENCIA / FATORIAL)
               ADD TERMO TO RESULTADO-COSSENO
               MULTIPLY -1 BY SINAL
           END-PERFORM.

       CALCULO-POTENCIA.
           MOVE 1 TO RESULTADO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > EXPOENTE
               MULTIPLY BASE BY RESULTADO
           END-PERFORM.

       CALCULO-FATORIAL.
           MOVE 1 TO RESULTADO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUMERO
               MULTIPLY I BY RESULTADO
           END-PERFORM.

       END PROGRAM PROG11.
