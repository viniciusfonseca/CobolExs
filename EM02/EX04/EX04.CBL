        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM02EX04.
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
            SELECT CADESTOQ ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT RELESTOQ ASSIGN TO DISK.
        
        DATA DIVISION.
            FILE SECTION.

            FD CADESTOQ
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADESTOQ.DAT".
            01 REGESTOQ.
               02 COD-ENT   PIC 9(05).
               02 NOME-ENT  PIC X(15).
               02 QTDE-ENT  PIC 9(05).
               02 PRECO-ENT PIC 9(06)V99.

            FD RELESTOQ
               LABEL RECORD IS OMITTED.
            01 REGIMPR           PIC X(80).

            WORKING-STORAGE SECTION.
            77 FIM-ARQ  PIC X(03)    VALUE  "NAO".
            77 CT-LIN   PIC 9(02)    VALUE     25.
            77 CT-PAG   PIC 9(02)    VALUE ZEROES.

            77 PREC-AUX PIC 9(06)V99 VALUE ZEROES.
            77 PRE-AUX2 PIC 9(06)V99 VALUE ZEROES.
            77 QTDE-AUX PIC 9(05)    VALUE ZEROES.

            01 CAB-01.
               02 FILLER     PIC X(06) VALUE     "DATA: ".
               02 DATE-PRINT PIC X(10) VALUE "99/99/9999".
               02 FILLER     PIC X(06) VALUE       SPACES.
               02 FILLER     PIC X(32) VALUE
                       "RELATORIO DE MATERIAL EM ESTOQUE".
               02 FILLER     PIC X(19) VALUE       SPACES.
               02 FILLER     PIC X(05) VALUE      "PAG. ".
               02 PAG-OUT    PIC Z9.

            01 CAB-02.
               02 FILLER     PIC X(06) VALUE     "CODIGO".
               02 FILLER     PIC X(02) VALUE       SPACES.

               02 FILLER     PIC X(04) VALUE       "NOME".
               02 FILLER     PIC X(11) VALUE       SPACES.
               02 FILLER     PIC X(02) VALUE       SPACES.

               02 FILLER     PIC X(15) VALUE 
                                        "QTDE DO ESTOQUE".
               02 FILLER     PIC X(02) VALUE       SPACES.

               02 FILLER     PIC X(14) VALUE
                                         "CUSTO UNITARIO".
               02 FILLER     PIC X(02) VALUE       SPACES.

               02 FILLER     PIC X(11) VALUE 
                                            "CUSTO TOTAL".
               02 FILLER     PIC X(11) VALUE       SPACES.
            
            01 DETALHE.
               02 COD-OUT    PIC 9(05).
               02 FILLER     PIC X(01) VALUE       SPACES.
               02 FILLER     PIC X(02) VALUE       SPACES.

               02 NOME-OUT   PIC X(15).
               02 FILLER     PIC X(02) VALUE       SPACES.

               02 QTDE-OUT   PIC ZZ.ZZ9.
               02 FILLER     PIC X(09) VALUE       SPACES.
               02 FILLER     PIC X(02) VALUE       SPACES.

               02 PRECO-OUT  PIC ZZZ.ZZ9,99.
               02 FILLER     PIC X(04) VALUE       SPACES.
               02 FILLER     PIC X(02) VALUE       SPACES.

               02 PRECO-TOT  PIC Z.ZZZ.ZZ9,99.
               02 FILLER     PIC X(10) VALUE       SPACES.

            01 CAB-03.
                02 FILLER    PIC X(16) VALUE 
                                       "QUANTIDADE TOTAL".
                02 FILLER    PIC X(02) VALUE       SPACES.
                02 FILLER    PIC X(11) VALUE 
                                            "PRECO MEDIO".
                02 FILLER    PIC X(02) VALUE       SPACES.
                02 FILLER    PIC X(11) VALUE
                                            "PRECO TOTAL".
                02 FILLER    PIC X(02) VALUE       SPACES.
                02 FILLER    PIC X(36) VALUE       SPACES.

            01 DET-FIM.
                02 QTDE-TOT  PIC Z.ZZZ.ZZ9.
                02 FILLER    PIC X(07) VALUE       SPACES.
                02 FILLER    PIC X(02) VALUE       SPACES.

                02 PREC-MED  PIC ZZZ.ZZ9,99.
                02 FILLER    PIC X(01) VALUE       SPACES.
                02 FILLER    PIC X(02) VALUE       SPACES.

                02 PREC-TOT  PIC ZZ.ZZZ.ZZ9,99.
                02 FILLER    PIC X(36) VALUE       SPACES.


        PROCEDURE DIVISION.
        PGM.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO.
            MOVE "02/10/2016" TO DATE-PRINT.
            OPEN INPUT  CADESTOQ
                 OUTPUT RELESTOQ.
            PERFORM LEITURA.

        LEITURA.
            READ CADESTOQ AT END MOVE "SIM" TO FIM-ARQ.
        
        PRINCIPAL.
            PERFORM IMPRESSAO.
            PERFORM LEITURA.
        
        IMPRESSAO.
            IF CT-LIN GREATER THAN 24
                PERFORM CABECALHO.
            PERFORM IMPDET.

        IMPDET.
            MULTIPLY PRECO-ENT BY QTDE-ENT GIVING PRE-AUX2.

            MOVE COD-ENT   TO   COD-OUT  .
            MOVE NOME-ENT  TO   NOME-OUT .
            MOVE QTDE-ENT  TO   QTDE-OUT .
            MOVE PRECO-ENT TO   PRECO-OUT.
            MOVE PRE-AUX2  TO   PRECO-TOT.

            WRITE REGIMPR FROM DETALHE AFTER ADVANCING 1 LINE.

            ADD  PRE-AUX2  TO PREC-AUX.
            ADD  QTDE-ENT  TO QTDE-AUX.
            ADD  1         TO CT-LIN  .

        CABECALHO.
            ADD   1       TO   CT-PAG .
            MOVE  CT-PAG  TO   PAG-OUT.
            MOVE  SPACES  TO   REGIMPR.

            WRITE REGIMPR             AFTER ADVANCING   PAGE .
            WRITE REGIMPR FROM CAB-01 AFTER ADVANCING 1 LINE .
            WRITE REGIMPR FROM CAB-02 AFTER ADVANCING 2 LINES.

            MOVE  ZEROES  TO   CT-LIN.

        IMPRIMEFIM.
            ADD   1       TO   CT-PAG .
            MOVE  CT-PAG  TO   PAG-OUT.
            MOVE  SPACES  TO   REGIMPR.

            WRITE REGIMPR              AFTER ADVANCING   PAGE.
            WRITE REGIMPR FROM CAB-01  AFTER ADVANCING 1 LINE.

            MOVE   PREC-AUX TO PREC-TOT.
            MOVE   QTDE-AUX TO QTDE-TOT.
            DIVIDE PREC-AUX BY QTDE-AUX GIVING PREC-MED.

            WRITE REGIMPR FROM CAB-03  AFTER ADVANCING 2 LINES.
            WRITE REGIMPR FROM DET-FIM AFTER ADVANCING 1 LINE.
            
        TERMINO.
            PERFORM IMPRIMEFIM.
            CLOSE CADESTOQ
                  RELESTOQ.