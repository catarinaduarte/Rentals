      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENTALS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL FIC ASSIGN TO "RENTALS.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS FS
           RECORD KEY IS FS-CHAVE.

           SELECT OPTIONAL EXPORT ASSIGN TO "RENTALS.HTML"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FIC.
       01 REGISTO.
           05 FS-CHAVE.
               10 FS-COD             PIC 9(09) BLANK WHEN ZEROS.
           05 FS-RENTAL               PIC 9(20).
           05 FS-ENTREGA            PIC 9(20).
            05 FS-LIVRO              PIC 9(20).
           05 FS-CLIENTE            PIC 9(20).
           05 FS-SITUACAO          PIC X(20).

       FD EXPORT.
       01 REGISTO1.
           05 LINHAS                 PIC X(150).

       WORKING-STORAGE SECTION.
       01 WS-MENU.
           05 FILLER                 PIC X(09) VALUES "RENTALS".
           05 WS-OP                  PIC X(20) VALUES SPACES.
       77 FS                         PIC 9(02).
           88 FS-OK                  VALUES 0.
           88 FS-CANCELA             VALUES 99.
           88 FS-NAO-EXISTE          VALUES 35.
      * CONTROLO DA LINHA
       77 WS-NUML                    PIC 9(02) VALUES 0.
      * CONTROLO DA COLUNA
       77 WS-NUMC                    PIC 9(02) VALUES 0.
       77 WS-ESTADO                  PIC X(30) VALUES SPACES.
       77 WS-OPCAO                   PIC 9 VALUES 9.
           88 INSERIR                VALUES 1.
           88 CONSULTAR              VALUES 2.
           88 ALTERAR                VALUES 3.
           88 ELIMINAR               VALUES 4.
           88 LISTAR                 VALUES 5.
           88 EXPORTAR               VALUES 6.
           88 SAIR                   VALUES 0.
       77 WS-MSG-ERRO                PIC X(80) VALUES SPACES.
       77 WS-REPETIR                 PIC X.
           88 SIM                    VALUES "S","s".
       77 WS-LINHA                   PIC 9(02).
      * A BIBLIOTECA DE AMBIENTE DE SCREEN
       COPY SCREENIO.
       SCREEN SECTION.
       01 CLS BLANK SCREEN.
       01 LAYOUT-INICIAL.
           05 PARTE-1.
               10 BLANK SCREEN.
               10 LINE 1 COL 1 ERASE EOL BACKGROUND-COLOR 1.
               10 LINE WS-NUML COL 1 ERASE EOL BACKGROUND-COLOR 1.
           05 CABECALHO.
               10 LINE 1 COL 1 PIC X(20) FROM WS-MENU HIGHLIGHT
               FOREGROUND-COLOR 6 BACKGROUND-COLOR 1.
           05 ESTADO.
               10 LINE WS-NUML COL 1 ERASE EOL PIC X(30) FROM WS-ESTADO
               HIGHLIGHT FOREGROUND-COLOR 6 BACKGROUND-COLOR 1.
       01 LAYOUT-MENU.
           05 LINE 5  COL 50 VALUE "A BIBLIOTECA DA VILA".
           05 LINE 7  COL 50 VALUE "1-INSERIR".
           05 LINE 8  COL 50 VALUE "2-CONSULTAR".
           05 LINE 9 COL 50  VALUE "3-ALTERAR".
           05 LINE 10 COL 50 VALUE "4-ELIMINAR".
           05 LINE 11 COL 50 VALUE "5-LISTAR".
           05 LINE 12 COL 50 VALUE "6-EXPORTAR".
           05 LINE 13 COL 50 VALUE "0-DEVOLVER".
           05 LINE 17 COL 50 VALUE "DIGITE A SUA ESCOLHA:".
           05 LINE 17 COL PLUS 1 USING WS-OPCAO AUTO.
       01 LAYOUT-REGISTO.
           05 WS-CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COL 10 VALUE "COD RENTALS".
               10 COLUMN PLUS 2 PIC 9(09) USING FS-COD BLANK WHEN ZEROS.
           05 WS-DADOS.
               10 LINE 11 COL 10 VALUE "DATA RENTAL:".
               10 COL PLUS 2 PIC 9(20) USING FS-RENTAL.
               10 LINE 12 COL 10 VALUE "DATA ENTREGA ".
               10 COL PLUS 2 PIC 9(20) USING FS-ENTREGA.
               10 LINE 12 COL 10 VALUE "COD LIVRO ".
               10 COL PLUS 2 PIC 9(20) USING FS-LIVRO.
               10 LINE 12 COL 10 VALUE "COD FS-CLIENTE ".
               10 COL PLUS 2 PIC 9(20) USING FS-CLIENTE.
               10 LINE 12 COL 10 VALUE "COD FS-SITUACAO ".
               10 COL PLUS 2 PIC 9(20) USING FS-SITUACAO.

       01 LAYOUT-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COL 1 PIC X(80) FROM WS-MSG-ERRO.
       PROCEDURE DIVISION.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESC_DELAY' TO '25'.

           ACCEPT WS-NUML FROM LINES.
           ACCEPT WS-NUMC FROM COLUMNS.

           OPEN I-O FIC.

           PERFORM UNTIL SAIR
               MOVE "MENU" TO WS-OP
               MOVE "SELECIONE A SUA OPCAO" TO WS-ESTADO
               MOVE 0 TO WS-OPCAO
               DISPLAY LAYOUT-INICIAL
               ACCEPT LAYOUT-MENU
               EVALUATE TRUE
                   WHEN INSERIR
                       PERFORM INICIO-INSERIR THRU FIM-INSERIR
                   WHEN CONSULTAR
                       PERFORM INICIO-CONSULTAR THRU FIM-CONSULTAR
                   WHEN ALTERAR
                       PERFORM INICIO-ALTERAR THRU FIM-ALTERAR
                   WHEN ELIMINAR
                       PERFORM INICIO-ELIMINAR THRU FIM-ELIMINAR
                   WHEN LISTAR
                       PERFORM INICIO-LISTAR THRU FIM-LISTAR
                   WHEN EXPORTAR
                       PERFORM INICIO-EXPORTAR THRU FIM-EXPORTAR
                   WHEN SAIR
                       CLOSE FIC
                       STOP RUN
               END-EVALUATE
           END-PERFORM.

       INICIO-INSERIR.
           MOVE "INSERIR" TO WS-OP.
           MOVE "ESC PARA SAIR" TO WS-ESTADO.
           DISPLAY LAYOUT-INICIAL.
           MOVE SPACES TO REGISTO.
       CICLO-INSERIR.
           ACCEPT LAYOUT-REGISTO.
           IF(COB-CRT-STATUS=COB-SCR-ESC) THEN
               GO FIM-INSERIR
           END-IF.
           IF (FS-RENTAL EQUALS SPACES) THEN
               MOVE "NOME E UM CAMPO OBRIGATORIO" TO WS-MSG-ERRO
               PERFORM MOSTRA-ERRO
               GO CICLO-INSERIR
           END-IF.
      *----------------
           WRITE REGISTO
            INVALID KEY
             DISPLAY "COD AUTORES JA EXISTE. ATENCAO !" AT 2301
             ACCEPT OMITTED AT 2401
             PERFORM MOSTRA-ERRO
             MOVE ZEROS TO FS-CHAVE
           END-WRITE.
           GO INICIO-INSERIR.
       FIM-INSERIR.

       INICIO-CONSULTAR.
           MOVE "CONSULTA" TO WS-OP.
           MOVE "ESC PARA SAIR" TO WS-ESTADO.
           DISPLAY LAYOUT-INICIAL.
       CICLO-CONSULTAR.
           MOVE SPACES TO REGISTO.
           DISPLAY LAYOUT-REGISTO.
           ACCEPT WS-CHAVE.
           IF(NOT COB-CRT-STATUS=COB-SCR-ESC) THEN
               READ FIC
                 INVALID KEY
                 DISPLAY "COD AUTORES INEXISTENTE!" AT 2301
                 ACCEPT OMITTED AT 2401
               END-READ
           ELSE
               MOVE 99 TO FS
           END-IF.
           IF(FS-CANCELA) THEN
               GO FIM-CONSULTAR
           END-IF.
           IF (FS-OK) THEN
               DISPLAY WS-DADOS
               MOVE "ENTER PARA CONTINUAR" TO WS-MSG-ERRO
               PERFORM MOSTRA-ERRO
               ACCEPT OMITTED AT 2301
           END-IF.
           GO CICLO-CONSULTAR.
       FIM-CONSULTAR.

       INICIO-ALTERAR.
           MOVE "ALTERAR" TO WS-OP.
           MOVE "ESC PARA SAIR" TO WS-ESTADO.
           DISPLAY LAYOUT-INICIAL.
       CICLO-ALTERAR.
           MOVE SPACES TO REGISTO.
           DISPLAY LAYOUT-REGISTO.
           ACCEPT WS-CHAVE.
           IF(NOT COB-CRT-STATUS=COB-SCR-ESC) THEN
               READ FIC
                 INVALID KEY
                 DISPLAY "COD AUTORES INEXISTENTE!" AT 2301
                 ACCEPT OMITTED AT 2401
               END-READ
           ELSE
               MOVE 99 TO FS
           END-IF.
           IF(FS-CANCELA) THEN
               GO FIM-ALTERAR
           END-IF.
           IF (FS-OK) THEN
               ACCEPT WS-DADOS
               IF(COB-CRT-STATUS=COB-SCR-ESC) THEN
                  GO CICLO-ALTERAR
               END-IF
           ELSE
               GO CICLO-ALTERAR
           END-IF.
           REWRITE REGISTO
            INVALID KEY
            MOVE "ERRO AO ALTERAR" TO WS-MSG-ERRO
            PERFORM MOSTRA-ERRO
            NOT INVALID KEY
            CONTINUE
           END-REWRITE.
           GO CICLO-ALTERAR.
       FIM-ALTERAR.


       INICIO-ELIMINAR.
           MOVE "ELIMINAR" TO WS-OP.
           MOVE "ESC PARA SAIR" TO WS-ESTADO.
           DISPLAY LAYOUT-INICIAL.
           MOVE SPACES TO REGISTO.
           DISPLAY LAYOUT-REGISTO.
           ACCEPT WS-CHAVE.
           IF(NOT COB-CRT-STATUS=COB-SCR-ESC) THEN
               READ FIC
                 INVALID KEY
                 DISPLAY "COD AUTORES INEXISTENTE!" AT 2301
                 ACCEPT OMITTED AT 2401
               END-READ
           ELSE
               MOVE 99 TO FS
           END-IF.
           IF(FS-CANCELA) THEN
               GO FIM-ELIMINAR
           END-IF.
           IF (NOT FS-OK) THEN
               GO FIM-ELIMINAR
           END-IF.
           DISPLAY WS-DADOS.
           MOVE "N" TO WS-REPETIR.
           DISPLAY "CONFIRMA ELIMINAR REGISTO?(S/N):" AT 2301
           ACCEPT WS-REPETIR AT 2332.
           IF (NOT SIM) THEN
               GO FIM-ELIMINAR
           END-IF.
           DELETE FIC
            INVALID KEY
            DISPLAY "ERRO AO ELIMINAR" AT 2301
            ACCEPT OMITTED AT 2401
           END-DELETE.
       FIM-ELIMINAR.

       INICIO-LISTAR.
           DISPLAY CLS.
           DISPLAY "GESTAO DE RENTAL" AT 0101.
           DISPLAY "COD RENTAL         ENTREGA       LIVRO  " AT 0301
           FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "------------------------------------------------"
           AT 0401.
           MOVE 0 TO FS-COD.
           START FIC KEY > FS-COD
             INVALID KEY DISPLAY "FICHEIRO VAZIO" AT 0501
           END-START.
           MOVE 5 TO WS-LINHA.
           PERFORM UNTIL FS="10"
               READ FIC NEXT RECORD
               AT END
               CONTINUE
               NOT AT END
               DISPLAY FS-COD           LINE WS-LINHA COL 1
               DISPLAY FS-RENTAL         LINE WS-LINHA COL 11
               DISPLAY FS-ENTREGA   LINE WS-LINHA COL 29
               DISPLAY FS-LIVRO         LINE WS-LINHA COL 39
               DISPLAY FS-CLIENTE   LINE WS-LINHA COL 49
               DISPLAY FS-SITUACAO   LINE WS-LINHA COL 59
               ADD 1 TO WS-LINHA
               END-READ
           END-PERFORM.
           ADD 1 TO WS-LINHA.
           DISPLAY "FIM DA LISTAGEM. ENTER PARA CONTINUAR" LINE
           WS-LINHA COL 1.
           ADD 1 TO WS-LINHA.
           ACCEPT OMITTED LINE WS-LINHA COL 1.
       FIM-LISTAR.

       INICIO-EXPORTAR.
           DISPLAY CLS.
           DISPLAY "EXPORTAR PARA HTML" AT 0101.
      * ABRIR FICHEIRO EXPORT.
           OPEN OUTPUT EXPORT.
           STRING "<BODY BGCOLOR=CYAN><H1>GESTAO RENTALS</H1>"
           DELIMITED BY SIZE
           "<TABLE BORDER=1>" DELIMITED BY SIZE
           "<TR><TH>COD</TH><TH>NOME</TH><TH>NOME</TH></TR>"
           DELIMITED BY SIZE
           INTO REGISTO1.
           WRITE REGISTO1.
           MOVE LOW-VALUES TO REGISTO1.

           MOVE 0 TO FS-COD.
           START FIC KEY > FS-COD
               INVALID KEY DISPLAY "FICHEIRO VAZIO" AT 0501
           END-START.
           PERFORM UNTIL FS="10"
               READ FIC NEXT RECORD
               AT END
               CONTINUE
               NOT AT END
               STRING "<TR><TD>" DELIMITED BY SIZE
               FS-RENTAL  DELIMITED BY SIZE "</TD>" DELIMITED BY SIZE
               "<TD>" DELIMITED BY SIZE
               FS-ENTREGA DELIMITED BY SIZE "</TD>" DELIMITED BY SIZE
               "<TD>" DELIMITED BY SIZE
              FS-LIVRO DELIMITED BY SIZE "</TD></TR>" DELIMITED BY SIZE
            FS-CLIENTE DELIMITED BY SIZE "</TD></TR>" DELIMITED BY SIZE
            FS-SITUACAO DELIMITED BY SIZE "</TD></TR>"DELIMITED BY SIZE
               INTO REGISTO1
               WRITE REGISTO1
               END-READ
           END-PERFORM.
           MOVE "</TABLE></BODY>" TO REGISTO1.
           WRITE REGISTO1.
      * FECHAR O FICHEIRO EXPORT.
           CLOSE EXPORT.
           DISPLAY "EXPORTACAO CONCLUIDA!" AT 0301 FOREGROUND-COLOR 2
           HIGHLIGHT.
           ACCEPT OMITTED AT 0501.

       FIM-EXPORTAR.

       MOSTRA-ERRO.
           DISPLAY LAYOUT-ERRO.
           ACCEPT LAYOUT-ERRO.
           DISPLAY ESTADO.
       END PROGRAM RENTALS.
