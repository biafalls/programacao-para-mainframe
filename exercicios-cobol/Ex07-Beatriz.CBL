      ******************************************************************
      * Author: Beatriz Lima
      * Date: 31/10
      * Purpose: Programa que armazena em uma matriz o nome de 6
      *          disciplinas do Curso de ADS e mostre em tela
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX07-CURSOS.
       AUTHOR. BEATRIZ.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 TABELA-DISCIPLINAS.
           05 DISCIPLINA PIC X(30) OCCURS 6.

       77 INDICE PIC 9(01) VALUE 1.

       PROCEDURE DIVISION.
            DISPLAY 'INFORME O NOME DE 6 DISCIPLINAS DO CURSO DE ADS'

            PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 6
               DISPLAY 'DIGITE O NOME DA DISCIPLINA ' INDICE ': '
               ACCEPT DISCIPLINA(INDICE)
            END-PERFORM.

            DISPLAY ' '
            DISPLAY '--DICIPLINAS DE ADS--'
            PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 6
               DISPLAY DISCIPLINA(INDICE)
            END-PERFORM.

            STOP RUN.
       END PROGRAM EX07-CURSOS.
