        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM02EX01.
        AUTHOR.        VINICIUS ALVES.
        INSTALLATION.  FATEC-SP.
        DATE-WRITTEN.  02/10/2016.
        DATE-COMPILED. 02/10/2016.

        ENVIRONMENT DIVISION.
            CONFIGURATION SECTION.
            SOURCE-COMPUTER. IBM-PC.
            OBJECT-COMPUTER. IBM-PC.
            SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
            SELECT ARQSOC1  ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT ARQSOC2  ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT RELSOCIO ASSIGN TO DISK.

        DATA DIVISION.
            FILE SECTION.

            FD ARQSOC1
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQSOC1.DAT".
            01 CADSOC1.
               02 CODPAG1 PIC 9(02).
               02 NUMSOC1 PIC 9(06).
               02 NOMSOC1 PIC X(30).
               02 VALPAG1 PIC 9(09)V9(02).

            FD ARQSOC2
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQSOC2.DAT".
            01 CADSOC2.
               02 NUMSOC2 PIC 9(06).
               02 NOMSOC2 PIC X(30).
               02 VALPAG2 PIC 9(09)V9(02).

            FD RELSOCIO
               LABEL RECORD IS OMITTED.
            01 REGSOCIO   PIC X(80).

            WORKING-STORAGE SECTION.
            77 FIM-ARQ    PIC X(03)       VALUE  "NAO".
            77 CT-LIN     PIC 99          VALUE     30.
            77 CT-PAG     PIC 99          VALUE ZEROES.

            77 AUXTOT     PIC 99          VALUE ZEROES.
            77 AUXVAL     PIC 9(09)V9(02) VALUE ZEROES.

            01 CAB-01.
               02 FILLER  PIC X(73) VALUE            SPACES.
               02 FILLER  PIC X(05) VALUE           "PAG. ".
               02 PAG-OUT PIC Z9.

            01 CAB-02.
               02 FILLER PIC X(26) VALUE             SPACES.
               02 FILLER PIC X(27) VALUE
                              "RELACAO DE SOCIOS ATRASADOS".
               02 FILLER PIC X(27) VALUE             SPACES.
               
            01 CAB-03.
               02 FILLER PIC X(15) VALUE  "NUMERO DO SOCIO".
               02 FILLER PIC X(02) VALUE             SPACES.
               02 FILLER PIC X(13) VALUE    "NOME DO SOCIO".
               02 FILLER PIC X(17) VALUE             SPACES.
               02 FILLER PIC X(02) VALUE             SPACES.
               02 FILLER PIC X(18) VALUE 
                                       "VALOR DO PAGAMENTO".
               02 FILLER PIC X(13) VALUE             SPACES.

            01 CAB-04.
               02 FILLER PIC X(80) VALUE             SPACES.

            01 DETALHE.
               02 RELNUM PIC 999.999.
               02 FILLER PIC X(09) VALUE             SPACES.
               02 FILLER PIC X(02) VALUE             SPACES.

               02 RELNOM PIC X(30).
               02 FILLER PIC X(02) VALUE             SPACES.

               02 RELVAL PIC ZZZ.ZZZ.ZZ9,99.
               02 FILLER PIC X(04) VALUE             SPACES.
               02 FILLER PIC X(11) VALUE             SPACES.

            01 ROD-01.
               02 FILLER PIC X(27) VALUE 
                              "TOTAL DE SOCIOS ATRASADOS: ".
               02 R-AUXTOT PIC 99999.

            01 ROD-02.
               02 FILLER PIC X(22) VALUE
                                   "VALOR TOTAL ATRASADO: ".
               02 IMPVAL PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
        
        PROCEDURE DIVISION.
        PGM.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO.
            OPEN INPUT  ARQSOC1
                 OUTPUT ARQSOC2
                        RELSOCIO.
            PERFORM LEITURA.

        LEITURA.
            READ ARQSOC1 AT END MOVE "SIM" TO FIM-ARQ.

        PRINCIPAL.
            PERFORM SAIDA.
            PERFORM LEITURA.

        SAIDA.
            IF CODPAG1 EQUAL 1
                PERFORM GRAVACAO.
            IF CODPAG1 EQUAL 2
                PERFORM IMPRESSAO.

        GRAVACAO.
            MOVE NUMSOC1 TO NUMSOC2.
            MOVE NOMSOC1 TO NOMSOC2.
            MOVE VALPAG1 TO VALPAG2.
            WRITE CADSOC2.

        IMPRESSAO.
            IF CT-LIN GREATER THAN 29
               PERFORM CABECALHO.
            PERFORM IMPDET.

        CABECALHO.
            ADD 1 TO CT-PAG.
            MOVE CT-PAG TO PAG-OUT.
            MOVE SPACES TO REGSOCIO.
            MOVE ZEROES TO AUXTOT.
            MOVE ZEROES TO CT-LIN.

            WRITE REGSOCIO             AFTER ADVANCING   PAGE .
            WRITE REGSOCIO FROM CAB-01 AFTER ADVANCING 1 LINE .
            WRITE REGSOCIO FROM CAB-02 AFTER ADVANCING 2 LINES.
            WRITE REGSOCIO FROM CAB-03 AFTER ADVANCING 2 LINES.
            WRITE REGSOCIO FROM CAB-04 AFTER ADVANCING 1 LINES.

        IMPDET.
            ADD   VALPAG1 TO AUXVAL.
            ADD   1       TO AUXTOT.
            MOVE  AUXTOT  TO CT-LIN.
            MOVE  AUXTOT  TO R-AUXTOT.

            MOVE  NUMSOC1 TO RELNUM.
            MOVE  NOMSOC1 TO RELNOM.
            MOVE  VALPAG1 TO RELVAL.

            WRITE REGSOCIO FROM DETALHE AFTER ADVANCING 1 LINE.            

        RODAPE.
            MOVE  AUXVAL   TO   IMPVAL.
            WRITE REGSOCIO FROM ROD-01 AFTER ADVANCING 4 LINES.
            WRITE REGSOCIO FROM ROD-02 AFTER ADVANCING 1 LINE.

        TERMINO.
            PERFORM RODAPE.
            CLOSE ARQSOC1
                  ARQSOC2
                  RELSOCIO.
