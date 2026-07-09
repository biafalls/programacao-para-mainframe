      ******************************************************************
      * Author: Beatriz Lima
      * Date: 17/10/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG03.
       AUTHOR. BEATRIZ LIMA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NOME     PIC X(30).
       77 WRK-MATERIA  PIC X(15).
       77 WRK-NOTA     PIC 9(02)V9(02).
       PROCEDURE DIVISION.
           DISPLAY 'POR FAVOR, DIGITE SEU NOME'
           ACCEPT WRK-NOME

           DISPLAY 'DIGITE A MATERIA QUE VOCE ESTA CURSANDO'
           ACCEPT WRK-MATERIA

           DISPLAY 'INFORME A NOTA QUE VOCE TIROU NESTA DISCIPLINA'
           ACCEPT WRK-NOTA

           DISPLAY 'OLA ' WRK-NOME
           DISPLAY 'VOCE ESTA MATRICULADO NA MATERIA ' WRK-MATERIA
           DISPLAY 'E SUA NOTA FOI: ' WRK-NOTA.

           GOBACK.
            STOP RUN.
       END PROGRAM PROG03.
