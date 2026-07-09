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
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG12.
       AUTHOR. BEATRIZ LIMA.
       DATE-WRITTEN. 07-11-2025.
      *=================================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *=================================================================
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 PI                            PIC 9V99999999 VALUE 3.14159265.
       77 OPCAO-CALCULO                 PIC 9 VALUE 0.
       77 RESPOSTA-USUARIO              PIC X(01) VALUE "S".
       77 RESULTADO-AREA                PIC 9(05)V9(04).
       77 RESULTADO-PERIMETRO           PIC 9(05)V9(04).
       77 SEMIPERIMETRO                 PIC 9(05)V9(04).

       01 TABELA-ENTRADAS.
           05 DADOS-FIGURAS OCCURS 10 TIMES.
              10 LADO1-TAB             PIC 9(04)V9(02).
              10 LADO2-TAB             PIC 9(04)V9(02).
              10 LADO3-TAB             PIC 9(04)V9(02).
              10 RAIO-TAB              PIC 9(04)V9(02).

       77 INDICE-TABELA                PIC 9(02) VALUE 1.

       77 LADO1-TRIANGULO              PIC 9(04)V9(02).
       77 LADO2-TRIANGULO              PIC 9(04)V9(02).
       77 LADO3-TRIANGULO              PIC 9(04)V9(02).
       77 RAIO-CIRCULO                 PIC 9(04)V9(02).

       PROCEDURE DIVISION.
       INICIO-PROGRAMA.
           DISPLAY "=========================================="
           DISPLAY "     CALCULADORA GEOMETRICA EM COBOL"
           DISPLAY "=========================================="

           PERFORM CICLO-PRINCIPAL UNTIL RESPOSTA-USUARIO = "N" OR
           RESPOSTA-USUARIO = "n"
           DISPLAY "PROGRAMA ENCERRADO. OBRIGADO!"
           STOP RUN.

       CICLO-PRINCIPAL.
           DISPLAY "Escolha a operação desejada:"
           DISPLAY "1 - Area e perimetro do triangulo escaleno"
           DISPLAY "2 - Area do circulo"
           ACCEPT OPCAO-CALCULO

           EVALUATE OPCAO-CALCULO
               WHEN 1
                   PERFORM CALCULAR-TRIANGULO
               WHEN 2
                   PERFORM CALCULAR-CIRCULO
               WHEN OTHER
                   DISPLAY "Opção invalida! Digite 1 ou 2."
           END-EVALUATE

           DISPLAY "Deseja realizar outro calculo? (S/N)"
           ACCEPT RESPOSTA-USUARIO.

       CALCULAR-TRIANGULO.
           DISPLAY "Digite o primeiro lado do triangulo: "
           ACCEPT LADO1-TRIANGULO
           DISPLAY "Digite o segundo lado do triangulo: "
           ACCEPT LADO2-TRIANGULO
           DISPLAY "Digite o terceiro lado do triangulo: "
           ACCEPT LADO3-TRIANGULO

           MOVE LADO1-TRIANGULO TO LADO1-TAB (INDICE-TABELA)
           MOVE LADO2-TRIANGULO TO LADO2-TAB (INDICE-TABELA)
           MOVE LADO3-TRIANGULO TO LADO3-TAB (INDICE-TABELA)

           COMPUTE RESULTADO-PERIMETRO =
                   LADO1-TRIANGULO + LADO2-TRIANGULO + LADO3-TRIANGULO

           COMPUTE SEMIPERIMETRO = RESULTADO-PERIMETRO / 2

           COMPUTE RESULTADO-AREA =
              FUNCTION SQRT(
                  SEMIPERIMETRO *
                  (SEMIPERIMETRO - LADO1-TRIANGULO) *
                  (SEMIPERIMETRO - LADO2-TRIANGULO) *
                  (SEMIPERIMETRO - LADO3-TRIANGULO)
              )

           DISPLAY "------------------------------------------"
           DISPLAY "RESULTADOS DO TRIANGULO ESCALENO:"
           DISPLAY "Perimetro: " RESULTADO-PERIMETRO
           DISPLAY "Area:       " RESULTADO-AREA
           DISPLAY "------------------------------------------"

           ADD 1 TO INDICE-TABELA.

       CALCULAR-CIRCULO.
           DISPLAY "Digite o raio do circulo: "
           ACCEPT RAIO-CIRCULO

           MOVE RAIO-CIRCULO TO RAIO-TAB (INDICE-TABELA)

           COMPUTE RESULTADO-AREA = PI * (RAIO-CIRCULO ** 2)

           DISPLAY "------------------------------------------"
           DISPLAY "RESULTADO DO CIRCULO:"
           DISPLAY "Area: " RESULTADO-AREA
           DISPLAY "------------------------------------------"

           ADD 1 TO INDICE-TABELA.

       END PROGRAM PROG12.
