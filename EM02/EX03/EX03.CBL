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
            SELECT CADENT ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT RELSAI ASSIGN TO DISK.

        DATA DIVISION.

            FD CADENT
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADENT.DAT".
            01 REGENT.
               02 CODENT PIC 9(07).
               02 NOME   PIC X(30).
               02 SALENT PIC 9(05)V9(02).
               02 SEXENT PIC X.
                  88 FEMININO VALUE "f" "F".

            FD RELSAI
               LABEL RECORD IS OMITTED.
            01 REGSAI  PIC X(80).

            WORKING-STORAGE SECTION.
            77 FIM-ARQ PIC X(03) VALUE  "NAO".
            77 CT-LIN  PIC 99    VALUE     25.
            77 CT-PAG  PIC 999   VALUE ZEROES.

            77 AUXTOTSAL PIC 9(05)V9(02) VALUE ZEROES.

            01 CAB-01.
               02 FILLER  PIC X(72) VALUE  SPACES.
               02 FILLER  PIC X(05) VALUE "PAG. ".
               02 PAG-OUT PIC ZZ9.

            01 CAB-02.
               02 FILLER  PIC X(21) VALUE SPACES.
               02 FILLER  PIC X(38) VALUE 
                     "LISTAGEM DOS CLIENTES DO SEXO FEMININO".
               02 FILLER  PIC X(21) VALUE SPACES.

            01 CAB-03.
               02 FILLER  PIC X(23) VALUE SPACES.
               02 FILLER  PIC X(34) VALUE
                         "COM SALARIOS SUPERIORES A 5.000,00".
               02 FILLER  PIC X(23) VALUE SPACES.

            01 CAB-04.
               02 FILLER  PIC X(06) VALUE "CODIGO".
               02 FILLER  PIC X(01) VALUE   SPACES.
               02 FILLER  PIC X(02) VALUE   SPACES.

               02 FILLER  PIC X(04) VALUE   "NOME".
               02 FILLER  PIC X(26) VALUE   SPACES.
               02 FILLER  PIC X(02) VALUE   SPACES.

            01 DETALHE.
               02 CODREL  PIC 9(07).
               02 FILLER  PIC X(02) VALUE   SPACES.

               02 NOMEREL PIC X(30).
               02 FILLER  PIC X(02) VALUE   SPACES.

               02 SALREL  PIC ZZ.999,99.

            01 ROD-01.
               02 FILLER  PIC X(29) VALUE 
                   "TOTAL DE CLIENTES IMPRESSOS: ".
               02 TOTCLI  PIC ZZ.999.

            01 ROD-02.
               02 FILLER  PIC X(18) VALUE
                              "TOTAL DE SALARIO: ".
               02 TOTSAL  PIC ZZZ.ZZZ.ZZ9,99.

        PROCEDURE DIVISION.
        PGM.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO.
            OPEN INPUT  CADENT
                 OUTPUT RELSAI.
            PERFORM LEITURA.

        PRINCIPAL.
            PERFORM IMPRESSAO.
            PERFORM LEITURA.

        LEITURA.
            READ  AT END MOVE "SIM" TO FIM-ARQ.

        IMPRESSAO.
            IF CT-LIN GREATER THAN 19
                PERFORM CABECALHO.
            PERFORM IMPDET.

        CABECALHO.
            ADD  1      TO CT-PAG.
            MOVE CT-PAG TO PAG-OUT.
            MOVE ZEROES TO CT-LIN.
            MOVE SPACES TO RELSAI.

            WRITE REGSAI             AFTER ADVANCING   PAGE.
            WRITE REGSAI FROM CAB-01 AFTER ADVANCING 1 LINE.
            WRITE REGSAI FROM CAB-02 AFTER ADVANCING 1 LINE.
            WRITE REGSAI FROM CAB-03 AFTER ADVANCING 1 LINE.
            WRITE REGSAI FROM CAB-04 AFTER ADVANCING 1 LINE.

        IMPDET.
            IF FEMININO AND SALENT GREATER THAN 5000 THEN
                WRITE REGSAI FROM DETALHE
            END-IF.

        RODAPE.
            WRITE REGSAI FROM ROD-01 AFTER ADVANCING 1 LINE.
            WRITE REGSAI FROM ROD-02 AFTER ADVANCING 1 LINE.

        TERMINO.
            PERFORM RODAPE.
            CLOSE CADENT
                  RELSAI.