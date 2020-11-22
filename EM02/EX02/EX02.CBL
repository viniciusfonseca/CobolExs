        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM02EX02.
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
            SELECT CADCLI ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT RELCLI ASSIGN TO DISK.

        DATA DIVISION.
            FILE SECTION.
            FD CADCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCLI.DAT".
            01 IREGCLI.
               02 CPF-CLI  PIC 9(11).
               02 NOME-CLI PIC X(30).
               02 EST-CLI  PIC X(02).
               02 CID-CLI  PIC X(30).
               02 TEL-CLI.
                  03 TEL-E1 PIC 9(04).
                  03 TEL-E2 PIC 9(04).
               02 DATA-CLI PIC 9(08).

            FD RELCLI
               LABEL RECORD IS OMITTED.
            01 REGCLI PIC X(80).               

        WORKING-STORAGE SECTION.
        77 FIM-ARQ PIC X(03) VALUE  "NAO".
        77 CT-LIN  PIC 99    VALUE     26.
        77 CT-PAG  PIC 99    VALUE ZEROES.

        01 CAB-01.
           02 FILLER  PIC X(73) VALUE  SPACES.
           02 FILLER  PIC X(05) VALUE "PAG. ".
           02 PAG-OUT PIC 99.

        01 CAB-02.
           02 FILLER PIC X(32) VALUE              SPACES.
           02 FILLER PIC X(17) VALUE "TURISMAR TURISMOS".
           02 FILLER PIC X(31) VALUE              SPACES.

        01 CAB-03.
           02 FILLER PIC X(23) VALUE                 SPACES.
           02 FILLER PIC X(20) VALUE "CLIENTES NO PERIODO ".
           02 FILLER PIC X(15) VALUE      "DE: 2010 A 2011".
           02 FILLER PIC X(22) VALUE                 SPACES.

        01 CAB-04.
           02 FILLER PIC X(04) VALUE     "NOME".
           02 FILLER PIC X(26) VALUE     SPACES.
           02 FILLER PIC X(02) VALUE     SPACES.

           02 FILLER PIC X(06) VALUE   "ESTADO".
           02 FILLER PIC X(02) VALUE     SPACES.
           
           02 FILLER PIC X(08) VALUE "TELEFONE".
           02 FILLER PIC X(01) VALUE     SPACES.
           02 FILLER PIC X(02) VALUE     SPACES.
           02 FILLER PIC X(31) VALUE     SPACES.

        01 CAB-05.
           02 FILLER PIC X(80) VALUE ALL "-".

        01 CAB-06.
           02 FILLER PIC X(80) VALUE SPACES.

        01 DETALHE.
           02 RELNOM PIC X(30).
           02 FILLER PIC X(02) VALUE     SPACES.
           02 RELEST PIC X(02).
           02 FILLER PIC X(04) VALUE     SPACES.
           02 FILLER PIC X(02) VALUE     SPACES.
           02 RELTEL.
              03 RELTEL1 PIC 9(04).
              03 FILLER  PIC X(01) VALUE "-".
              03 RELTEL2 PIC 9(04).

        01 ROD-01.
           02 FILLER PIC X(19) VALUE "TOTAL DE CLIENTES: ".
           02 TOTCLI PIC 9999  VALUE ZEROES.
           02 FILLER PIC X(57) VALUE SPACES.

        PROCEDURE DIVISION.
        PGM.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO.
            OPEN INPUT  CADCLI
                 OUTPUT RELCLI.
            PERFORM LEITURA.

        LEITURA.
            READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.

        PRINCIPAL.
            PERFORM IMPRESSAO.
            PERFORM LEITURA.

        IMPRESSAO.
            IF CT-LIN GREATER THAN 25
                PERFORM CABECALHO.
            PERFORM IMPDET.
        
        CABECALHO.
            ADD  1      TO CT-PAG.
            MOVE ZEROES TO CT-LIN.
            MOVE SPACES TO REGCLI.
            MOVE CT-PAG TO PAG-OUT.
            
            WRITE REGCLI             AFTER ADVANCING   PAGE .
            WRITE REGCLI FROM CAB-01 AFTER ADVANCING 2 LINES.
            WRITE REGCLI FROM CAB-02 AFTER ADVANCING 1 LINE .
            WRITE REGCLI FROM CAB-03 AFTER ADVANCING 1 LINE .
            WRITE REGCLI FROM CAB-04 AFTER ADVANCING 1 LINE .
            WRITE REGCLI FROM CAB-05 AFTER ADVANCING 1 LINE .
            WRITE REGCLI FROM CAB-06 AFTER ADVANCING 1 LINE .

        IMPDET.
            MOVE NOME-CLI TO RELNOM.
            MOVE EST-CLI  TO RELEST.
            MOVE TEL-E1   TO RELTEL1.
            MOVE TEL-E2   TO RELTEL2.

            ADD  1        TO TOTCLI.
            ADD  1        TO CT-LIN.

            WRITE REGCLI FROM DETALHE AFTER ADVANCING 1 LINE.

        RODAPE.
            WRITE REGCLI FROM ROD-01 AFTER ADVANCING 4 LINES.

        TERMINO.
            PERFORM RODAPE.
            CLOSE CADCLI
                  RELCLI.