        IDENTIFICATION DIVISION.
        PROGRAM-ID. EM03EX06.
        AUTHOR. VINICIUS ALVES.
        INSTALLATION. FATEC-SP.
        DATE-WRITTEN. 23/11/2020.
        DATE-COMPILED.
        SECURITY. ONLY AUTHOR MAY MODIFY.
      * REMARKS.
        
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
            SOURCE-COMPUTER. IBM-PC.
            OBJECT-COMPUTER. IBM-PC.
            SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
        
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT CADVEND ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT TRAB   ASSIGN TO DISK.
            SELECT REL-PAG ASSIGN TO DISK.
            
        DATA DIVISION.
        FILE SECTION.
        
        FD CADVEND
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADVEND.DAT".
        01 REGENT.
           02 CODDEPENT PIC X.
           02 CODSECENT PIC X.
           02 CODENT    PIC 9(05).
           02 NOMEENT   PIC X(20).
           02 VENDEENT  PIC 9(07)V99.
           02 SALENTF   PIC 9(07)V99.
           02 DESCENT   PIC 9(05)V99.

        SD TRAB.
        01 REGTRAB.
           02 CODDEPTRA PIC X.
           02 CODSECTRA PIC X.
           02 CODTRAB   PIC 9(05).
           02 NOMETRAB  PIC X(20).
           02 VENDETRAB PIC 9(07)V99.
           02 SALTRABF  PIC 9(07)V99.
           02 DESCTRA   PIC 9(05)V99.
        
        FD REL-PAG
           LABEL RECORD IS OMITTED.
        01 RELREG    PIC X(80).

        WORKING-STORAGE SECTION.
        77 FIM-ARQ PIC XXX VALUE "NAO".

        77 CT-LIN  PIC 99 VALUE ZEROES.
        77 CT-PAG  PIC 99 VALUE ZEROES.

        77 PREVDEP PIC X VALUE SPACES.
        77 PREVSEC PIC X VALUE SPACES.

        77 MUDOUDEP PIC XXX VALUE "NAO".
        77 MUDOUSEC PIC XXX VALUE "NAO".

        77 AUXTOTSALDEP PIC 9(07)V99 VALUE ZEROES.
        77 AUXTOTSALSEC PIC 9(07)V99 VALUE ZEROES.

        77 AUXCOM      PIC 9(09)V99 VALUE ZEROES.
        77 AUXSALBR    PIC 9(09)V99 VALUE ZEROES.
        77 AUXSALLI    PIC 9(09)V99 VALUE ZEROES.
        77 AUXTOTSALLI PIC 9(09)V99 VALUE ZEROES.

        01 CAB-01.
           02 FILLER PIC X(25) VALUE SPACES.
           02 FILLER PIC X(22) VALUE "RELATORIO DE PAGAMENTO".
           02 FILLER PIC X(25) VALUE SPACES.
           02 FILLER PIC X(05) VALUE "PAG. ".
           02 PAGE-P PIC ZZ9   VALUE ZEROES.

        01 CAB-DEP.
           02 FILLER PIC X(14) VALUE "DEPARTAMENTO: ".
           02 DEPREL PIC X.
           02 FILLER PIC X(65) VALUE SPACES.

        01 CAB-SEC.
           02 FILLER PIC X(14) VALUE "SECAO:        ".
           02 DEPSEC PIC X.

        01 CAB-SP.
           02 FILLER PIC X(80) VALUE SPACES.

        01 CAB-COLS.
           02 FILLER PIC X(06) VALUE "CODIGO".
           02 FILLER PIC X     VALUE SPACES.

           02 FILLER PIC X(04) VALUE "NOME".
           02 FILLER PIC X(09) VALUE SPACES.

           02 FILLER PIC X(09) VALUE "SAL. FIXO".
           02 FILLER PIC XX    VALUE SPACES.

           02 FILLER PIC X(08) VALUE "COMISSAO".
           02 FILLER PIC XXXX  VALUE SPACES.

           02 FILLER PIC X(10) VALUE "SAL. BRUTO".
           02 FILLER PIC XX    VALUE SPACES.

           02 FILLER PIC X(08) VALUE "DESCONTO".
           02 FILLER PIC XXXX  VALUE SPACES.

           02 FILLER PIC X(12) VALUE "SAL. LIQUIDO".

        01 DET-FUNC.
           02 CODRELA   PIC 9(05).
           02 FILLER    PIC XX   VALUE SPACES.

           02 NOMEREL   PIC X(12).
           02 FILLER    PIC X    VALUE SPACES.

           02 SALFIXREL PIC ZZZZZZZ9,99 VALUE ZEROES.
           02 FILLER    PIC X           VALUE SPACES.

           02 COMREL    PIC ZZZZZZZ9,99 VALUE ZEROES.
           02 FILLER    PIC X           VALUE SPACES.

           02 SALBRREL  PIC ZZZZZZZ9,99 VALUE ZEROES.
           02 FILLER    PIC X           VALUE SPACES.

           02 DESCREL   PIC ZZZZZZZ9,99 VALUE ZEROES.
           02 FILLER    PIC X           VALUE SPACES.

           02 SALLIQREL PIC ZZZZZZZ9,99 VALUE ZEROES.

        01 FIMSEC.
           02 FILLER    PIC X     VALUE SPACES.
           02 FILLER    PIC X(28) VALUE "TOTAL DE SALARIOS DA SECAO: ".
           02 TOTSALSECREL PIC ZZZ.ZZZ.ZZZ,99.

        01 FIMDEP.
           02 FILLER    PIC X VALUE SPACES.
           02 FILLER    PIC X(35)
               VALUE "TOTAL DE SALARIOS DO DEPARTAMENTO: ".
           02 TOTSALDEPREL PIC ZZZ.ZZZ.ZZZ.ZZZ,99.

        PROCEDURE DIVISION.

        PGM.
            SORT TRAB
                 ASCENDING KEY CODDEPTRA
                               CODSECTRA
                 USING     CADVEND
                 OUTPUT PROCEDURE ROTPGM.

        ROTPGM SECTION.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO SECTION.
            OPEN OUTPUT REL-PAG.
            PERFORM LEITURA.
            MOVE CODDEPTRA TO PREVDEP.
            MOVE CODSECTRA TO PREVSEC.
            PERFORM CABECALHO.
            PERFORM IMPCABSEC.
            PERFORM IMPCOLS.

        LEITURA SECTION.
            RETURN TRAB AT END MOVE "SIM" TO FIM-ARQ.

        PRINCIPAL SECTION.
            PERFORM IMPRESSAO.
            PERFORM LEITURA.

        IMPRESSAO SECTION.
            IF CT-LIN GREATER THAN 39
               PERFORM CABECALHO.

            PERFORM VERIFICADEP.

            IF MUDOUDEP EQUAL "SIM"
                PERFORM IMPRODDEP
                PERFORM CABECALHO
                PERFORM IMPCABSEC
                PERFORM IMPCOLS
                PERFORM IMPDET
                EXIT SECTION
            END-IF.

            PERFORM VERIFICASEC.

            IF MUDOUSEC EQUAL "SIM"
                PERFORM IMPRODSEC
                PERFORM CABECALHO
                PERFORM IMPCABSEC
                PERFORM IMPCOLS
            END-IF.

            PERFORM IMPDET.

        IMPDET SECTION.
            ADD 1 TO CT-LIN.
            PERFORM CONTABILIZA.
            MOVE CODTRAB TO CODRELA.
            MOVE NOMETRAB TO NOMEREL.
            MOVE SALTRABF TO SALFIXREL.
            MOVE DESCTRA TO DESCREL.
            WRITE RELREG FROM DET-FUNC AFTER ADVANCING 1 LINE.

        CABECALHO SECTION.
            ADD 1 TO CT-PAG.
            MOVE CT-PAG TO PAGE-P.
            MOVE SPACES TO RELREG.
            MOVE CODDEPTRA TO DEPREL.

            WRITE RELREG               AFTER ADVANCING   PAGE.
            WRITE RELREG FROM CAB-01   AFTER ADVANCING 1 LINE.
            WRITE RELREG FROM CAB-DEP  AFTER ADVANCING 2 LINES.

            MOVE ZEROES TO CT-LIN.

        IMPCABSEC SECTION.
            MOVE  CODSECTRA TO DEPSEC.
            WRITE RELREG FROM CAB-SEC AFTER ADVANCING 1 LINE.

        IMPCOLS SECTION.
            WRITE RELREG FROM CAB-COLS AFTER ADVANCING 1 LINE.

        CONTABILIZA SECTION.
            IF VENDETRAB NOT GREATER THAN 1000 THEN
               MOVE 5 TO AUXCOM
            ELSE
               IF VENDETRAB NOT GREATER THAN 3000 THEN
                  MOVE 10 TO AUXCOM
               ELSE
                  MOVE 10 TO AUXCOM
               END-IF
            END-IF.

            DIVIDE 100 INTO AUXCOM.

            MULTIPLY VENDETRAB BY AUXCOM GIVING AUXCOM.
            MOVE AUXCOM TO COMREL.
            ADD AUXCOM TO SALTRABF GIVING AUXSALBR SALBRREL.
            SUBTRACT DESCTRA FROM AUXSALBR GIVING SALLIQREL AUXSALLI.
            ADD AUXSALLI TO AUXTOTSALSEC.
            ADD AUXSALLI TO AUXTOTSALDEP.

        VERIFICADEP SECTION.
            MOVE "NAO" TO MUDOUDEP.
            IF CODDEPTRA NOT EQUAL TO PREVDEP THEN
               MOVE CODDEPTRA TO PREVDEP
               MOVE "SIM" TO MUDOUDEP
            END-IF.
        
        VERIFICASEC SECTION.
            MOVE "NAO" TO MUDOUSEC.
            IF CODSECTRA NOT EQUAL TO PREVSEC THEN
               MOVE CODSECTRA TO PREVSEC
               MOVE "SIM" TO MUDOUSEC
            END-IF.

        IMPRODSEC SECTION.
            MOVE  AUXTOTSALSEC TO TOTSALSECREL.
            WRITE RELREG FROM FIMSEC AFTER ADVANCING 2 LINES.
            MOVE  ZEROES TO AUXTOTSALSEC.

        IMPRODDEP SECTION.
            IF AUXTOTSALDEP IS EQUAL TO ZEROES THEN EXIT SECTION. 
            MOVE  AUXTOTSALDEP TO TOTSALDEPREL.
            WRITE RELREG FROM FIMDEP AFTER ADVANCING 1 LINE.
            MOVE  ZEROES TO AUXTOTSALDEP.

        TERMINO SECTION.
            PERFORM IMPRODSEC.
            PERFORM IMPRODDEP.
            CLOSE REL-PAG.