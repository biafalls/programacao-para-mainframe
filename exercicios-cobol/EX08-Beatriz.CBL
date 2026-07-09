      ******************************************************************
      * Author: Beatriz Lima
      * Date: 31/10
      * Purpose: Programa que armazena em uma matriz com as notas P1, P2
      *          e Atividade de um aluno em 6 disciplinas
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX08-NOTAS.
       AUTHOR. BEATRIZ.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 TABELA-DISCIPLINAS.
           03 DISCIPLINA PIC X(30) OCCURS 6.

       01 TABELA-NOTAS.
           03 NOTAS OCCURS 6.
                 05 PROVA1     PIC 9(02).
                 05 PROVA2     PIC 9(02).
                 05 ATIVIDADE  PIC 9(02).

       77 INDICE PIC 9(01) VALUE 1.

       PROCEDURE DIVISION.
            PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 6
               DISPLAY 'DIGITE O NOME DA DISCIPLINA ' INDICE ': '
               ACCEPT DISCIPLINA(INDICE)

               DISPLAY 'INFORME A NOTA DA PROVA 1: '
               ACCEPT PROVA1 OF TABELA-NOTAS(INDICE)

               DISPLAY 'INFORME A NOTA DA PROVA 2: '
               ACCEPT PROVA2 OF TABELA-NOTAS(INDICE)

               DISPLAY 'INFORME A NOTA DA ATIVIDADE: '
               ACCEPT ATIVIDADE OF TABELA-NOTAS(INDICE)

            END-PERFORM.

            DISPLAY ' '
            PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > 6
               DISPLAY 'DISCIPLINA ' INDICE ': 'DISCIPLINA(INDICE)
               DISPLAY 'PROVA 1: ' PROVA1(INDICE)
               DISPLAY 'PROVA 2: ' PROVA2(INDICE)
               DISPLAY 'ATIVIDADE: ' ATIVIDADE(INDICE)
               DISPLAY ' '
            END-PERFORM.

            STOP RUN.
       END PROGRAM EX08-NOTAS.
