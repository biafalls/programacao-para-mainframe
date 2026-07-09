      ******************************************************************
      * Author: Beatriz Lima
      * Date: 07/11/2025
      * Purpose: Programa que faz um dos cálculos a seguir:
      *          1) Área e perímetro de um triângulo escaleno ou
      *          2) área de um círculo.
      *        O usuário fará a entrada das opções acima via teclado, e,
      *        informa também via teclado, em seguida, os valores de entrada,
      *        também via teclado. Você armazenará os valores em tabela
      *        interna. No final, deve perguntar se o usuário quer continuar
      *        ou encerrar o programa
      * Tectonics: cobc
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO12.
       AUTHOR. BEATRIZ LIMA.
       DATE-WRITTEN. 07-11-2025.
      *=================================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *=================================================================
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 PI                   PIC 9V999999 VALUE 3.141592.
       77 OPCAO-CALCULO        PIC 9 VALUE 0.
       77 RESPOSTA-USUARIO     PIC X(01) VALUE "S".

       * Variáveis numéricas principais
       77 RESULTADO-AREA       COMP-2 VALUE 0.
       77 RESULTADO-PERIMETRO  COMP-2 VALUE 0.
       77 SEMIPERIMETRO        COMP-2 VALUE 0.
       77 AREA-VALOR           COMP-2 VALUE 0.
       77 ESTIMATIVA           COMP-2 VALUE 0.
       77 NOVA-ESTIMATIVA      COMP-2 VALUE 0.
       77 I                    PIC 9(02) VALUE 0.

       * Variáveis de entrada
       77 LADO1-TRIANGULO      COMP-2 VALUE 0.
       77 LADO2-TRIANGULO      COMP-2 VALUE 0.
       77 LADO3-TRIANGULO      COMP-2 VALUE 0.
       77 RAIO-CIRCULO         COMP-2 VALUE 0.

       * Tabela interna para armazenar dados
       01 TABELA-ENTRADAS.
          05 DADOS-FIGURAS OCCURS 10 TIMES.
             10 LADO1-TAB    COMP-2.
             10 LADO2-TAB    COMP-2.
             10 LADO3-TAB    COMP-2.
             10 RAIO-TAB     COMP-2.

       77 INDICE-TABELA        PIC 9(02) VALUE 1.
      *=================================================================
       PROCEDURE DIVISION.
       INICIO-PROGRAMA.
           PERFORM UNTIL RESPOSTA-USUARIO = "N" OR
           RESPOSTA-USUARIO = "n"
               DISPLAY "--------------------------------------------"
               DISPLAY "Escolha o calculo desejado:"
               DISPLAY "1 - Area e perimetro de um triangulo escaleno"
               DISPLAY "2 - Area de um circulo"
               ACCEPT OPCAO-CALCULO
               DISPLAY "--------------------------------------------"

               EVALUATE OPCAO-CALCULO
                   WHEN 1
                       PERFORM CALCULO-TRIANGULO
                   WHEN 2
                       PERFORM CALCULO-CIRCULO
                   WHEN OTHER
                       DISPLAY "Opcao invalida."
               END-EVALUATE

               DISPLAY "Deseja realizar outro calculo? (S/N): "
               ACCEPT RESPOSTA-USUARIO
           END-PERFORM

           DISPLAY "Programa encerrado."
           STOP RUN.

       CALCULO-TRIANGULO.
           DISPLAY "Digite o primeiro lado do triangulo: "
           ACCEPT LADO1-TRIANGULO
           DISPLAY "Digite o segundo lado do triangulo: "
           ACCEPT LADO2-TRIANGULO
           DISPLAY "Digite o terceiro lado do triangulo: "
           ACCEPT LADO3-TRIANGULO

           MOVE LADO1-TRIANGULO TO LADO1-TAB(INDICE-TABELA)
           MOVE LADO2-TRIANGULO TO LADO2-TAB(INDICE-TABELA)
           MOVE LADO3-TRIANGULO TO LADO3-TAB(INDICE-TABELA)

           COMPUTE RESULTADO-PERIMETRO =
               LADO1-TRIANGULO + LADO2-TRIANGULO + LADO3-TRIANGULO

           COMPUTE SEMIPERIMETRO = RESULTADO-PERIMETRO / 2

           COMPUTE AREA-VALOR =
               SEMIPERIMETRO *
               (SEMIPERIMETRO - LADO1-TRIANGULO) *
               (SEMIPERIMETRO - LADO2-TRIANGULO) *
               (SEMIPERIMETRO - LADO3-TRIANGULO)

           IF AREA-VALOR <= 0 THEN
           DISPLAY "Os lados informados nao formam um triangulo valido."
           ELSE

               COMPUTE ESTIMATIVA = AREA-VALOR / 2
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                   COMPUTE NOVA-ESTIMATIVA =
                       (ESTIMATIVA + (AREA-VALOR / ESTIMATIVA)) / 2
                   MOVE NOVA-ESTIMATIVA TO ESTIMATIVA
               END-PERFORM

               MOVE ESTIMATIVA TO RESULTADO-AREA

               DISPLAY "--------------------------------------------"
               DISPLAY "RESULTADOS DO TRIANGULO ESCALENO:"
               DISPLAY "Perimetro: " RESULTADO-PERIMETRO
               DISPLAY "Area aproximada: " RESULTADO-AREA
               DISPLAY "--------------------------------------------"
           END-IF.

       CALCULO-CIRCULO.
           DISPLAY "Digite o raio do circulo: "
           ACCEPT RAIO-CIRCULO

           MOVE RAIO-CIRCULO TO RAIO-TAB(INDICE-TABELA)

           COMPUTE RESULTADO-AREA = PI * RAIO-CIRCULO * RAIO-CIRCULO

           DISPLAY "--------------------------------------------"
           DISPLAY "RESULTADOS DO CIRCULO:"
           DISPLAY "Raio: " RAIO-CIRCULO
           DISPLAY "Area: " RESULTADO-AREA
           DISPLAY "--------------------------------------------".

       END PROGRAM DESAFIO12.
