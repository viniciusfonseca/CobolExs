        IDENTIFICATION DIVISION.
        PROGRAM-ID. EM03EX04.
        AUTHOR. VINICIUS ALVES.
        INSTALLATION. FATEC-SP.
        DATE-WRITTEN. 06/11/2016.
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
            SELECT CADCLI ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT TRAB   ASSIGN TO DISK.
            SELECT RELGER ASSIGN TO DISK.
            
        DATA DIVISION.
        FILE SECTION.
        
        FD CADCLI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCLI.DAT".
        01 REGENT.
           02 CODENT PIC 999.
           02 CPFENT.
              03 CPFEN1 PIC 999.
              03 CPFEN2 PIC 999.
              03 CPFEN3 PIC 999.
              03 CPFEN4 PIC 99.
           02 NOMEEN PIC X(30).
           02 ESTAEN PIC XX.
           02 CIDAEN PIC X(30).
           02 EMAILE PIC X(30).
           
        SD TRAB.
        01 REGTRA.
           02 CODTRA PIC 999.
           02 CPFTRA.
              03 CPFTR1 PIC 999.
              03 CPFTR2 PIC 999.
              03 CPFTR3 PIC 999.
              03 CPFTR4 PIC 99.
           02 NOMETR PIC X(30).
           02 ESTATR PIC XX.
           02 CIDATR PIC X(30).
           02 EMAILT PIC X(30).
           
        FD RELGER
           LABEL RECORD IS OMITTED.
        01 REGREL PIC X(80).
        
        WORKING-STORAGE SECTION.
        77 CT-LIN  PIC 99    VALUE 20.
        77 CT-PAG  PIC 99    VALUE ZEROES.
        77 FIM-ARQ PIC XXX   VALUE "NAO".

        77 PREVEST PIC XX    VALUE SPACES.
        77 PREVCID PIC X(30) VALUE SPACES.
        77 MUDOUES PIC XXX   VALUE "NAO".
        77 MUDOUCI PIC XXX   VALUE "NAO".
        
        01 CAB-01.
           02 FILLER PIC X(17) VALUE SPACES.
           02 FILLER PIC X(19) VALUE "RELACAO DE CLIENTES". 
           02 FILLER PIC X(18) VALUE " POR ESTADO/CIDADE".
           02 FILLER PIC X(18) VALUE SPACES.
           02 FILLER PIC X(05) VALUE "PAG. ".
           02 PAG-OU PIC ZZ9   VALUE ZEROES.

        01 CAB-02.
           02 FILLER PIC X(08) VALUE "ESTADO: ".
           02 RELEST PIC X(30) VALUE SPACES.
           02 FILLER PIC X(42) VALUE SPACES.

        01 CAB-03.
           02 FILLER PIC X(08) VALUE "CIDADE: ".
           02 RELCID PIC X(30) VALUE SPACES.
           02 FILLER PIC X(42) VALUE SPACES.

        01 CAB-04.
           02 FILLER PIC X(05) VALUE SPACES.
           02 FILLER PIC XXX   VALUE "CPF".
           02 FILLER PIC X(06) VALUE SPACES.
           02 FILLER PIC XXX   VALUE SPACES.

           02 FILLER PIC X(04)  VALUE "NOME".
           02 FILLER PIC X(26)  VALUE SPACES.
           02 FILLER PIC XXX    VALUE SPACES.

           02 FILLER PIC X(05) VALUE "EMAIL".
           02 FILLER PIC X(25) VALUE SPACES.

        01 DETALHE.
           02 CPFRE1 PIC 999.
           02 FILLER PIC X      VALUE ".".
           02 CPFRE2 PIC 999.
           02 FILLER PIC X      VALUE ".".
           02 CPFRE3 PIC 999.
           02 FILLER PIC X      VALUE "-".
           02 CPFRE4 PIC 99. 
           02 FILLER PIC XXX    VALUE SPACES.

           02 NOMERE PIC X(30) VALUE SPACES.
           02 FILLER PIC XXX   VALUE SPACES.

           02 EMAILR PIC X(30) VALUE SPACES.

        PROCEDURE DIVISION.

        PGM.
            SORT TRAB
                 ASCENDING KEY ESTATR
                               CIDATR
                               CPFTR1 CPFTR2 CPFTR3 CPFTR4
                 USING CADCLI
                 OUTPUT PROCEDURE ROTSAIDA        
        STOP RUN.

        ROTSAIDA SECTION.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.

        INICIO SECTION.
            OPEN OUTPUT RELGER.
            PERFORM LEITURA.

        LEITURA SECTION.
            RETURN TRAB AT END MOVE "SIM" TO FIM-ARQ.

        PRINCIPAL SECTION.
            PERFORM IMPRESSAO.
            PERFORM LEITURA.

        VERIFICALU SECTION.
            MOVE "NAO" TO MUDOUES.
            MOVE "NAO" TO MUDOUCI.
            IF PREVEST NOT EQUAL ESTATR THEN
                MOVE ESTATR TO PREVEST
                MOVE "SIM"  TO MUDOUES
            END-IF.
            IF PREVCID NOT EQUAL CIDATR THEN
                MOVE CIDATR TO PREVCID
                MOVE CIDATR TO RELCID
                MOVE "SIM"  TO MUDOUCI
            END-IF.

        CABECALHO SECTION.
            ADD   1       TO   CT-PAG.
            MOVE  CT-PAG  TO   PAG-OU.
            MOVE  SPACES  TO   REGREL.

            PERFORM ATRIBUIEST.

            WRITE REGREL              AFTER ADVANCING   PAGE .
            WRITE REGREL  FROM CAB-01 AFTER ADVANCING 1 LINE .
            WRITE REGREL  FROM CAB-02 AFTER ADVANCING 2 LINES.
            WRITE REGREL  FROM CAB-04 AFTER ADVANCING 2 LINES.

            MOVE  ZEROES  TO   CT-LIN.

        IMPRESSAO SECTION.
            PERFORM VERIFICALU.
            IF CT-LIN GREATER THAN 19 OR MUDOUES EQUAL "SIM" 
                THEN PERFORM CABECALHO.
            PERFORM IMPDET.

        COPIAVAR SECTION.
            MOVE CPFTR1 TO CPFRE1.
            MOVE CPFTR2 TO CPFRE2.
            MOVE CPFTR3 TO CPFRE3.
            MOVE CPFTR4 TO CPFRE4.
            MOVE NOMETR TO NOMERE.
            MOVE EMAILT TO EMAILR.

        IMPDET SECTION.
            PERFORM COPIAVAR.
            IF MUDOUCI IS EQUAL TO "SIM" THEN
               PERFORM IMPCID
               MOVE "NAO" TO MUDOUCI
            END-IF.
            WRITE REGREL FROM DETALHE AFTER ADVANCING 1 LINE.
            ADD 1 TO CT-LIN.

        IMPCID SECTION.
            WRITE REGREL FROM CAB-03 AFTER ADVANCING 2 LINES.

        ATRIBUIEST SECTION.
                 IF PREVEST EQUAL "AC" THEN
                MOVE "ACRE"                TO RELEST
            ELSE IF PREVEST EQUAL "AP" THEN
                MOVE "AMAPA"               TO RELEST
            ELSE IF PREVEST EQUAL "AM" THEN
                MOVE "AMAZONAS"            TO RELEST
            ELSE IF PREVEST EQUAL "BA" THEN
                MOVE "BAHIA"               TO RELEST
            ELSE IF PREVEST EQUAL "CE" THEN
                MOVE "CEARA"               TO RELEST
            ELSE IF PREVEST EQUAL "DF" THEN
                MOVE "DISTRITO FEDERAL"    TO RELEST
            ELSE IF PREVEST EQUAL "ES" THEN
                MOVE "ESPIRITO SANTO"      TO RELEST
            ELSE IF PREVEST EQUAL "GO" THEN
                MOVE "GOIAS"               TO RELEST
            ELSE IF PREVEST EQUAL "MA" THEN
                MOVE "MARANHAO"            TO RELEST
            ELSE IF PREVEST EQUAL "MT" THEN
                MOVE "MATO GROSSO"         TO RELEST
            ELSE IF PREVEST EQUAL "MS" THEN
                MOVE "MATO GROSSO DO SUL"  TO RELEST
            ELSE IF PREVEST EQUAL "MG" THEN
                MOVE "MINAS GERAIS"        TO RELEST
            ELSE IF PREVEST EQUAL "PA" THEN
                MOVE "PARA"                TO RELEST
            ELSE IF PREVEST EQUAL "PB" THEN
                MOVE "PARAIBA"             TO RELEST
            ELSE IF PREVEST EQUAL "PR" THEN
                MOVE "PARANA"              TO RELEST
            ELSE IF PREVEST EQUAL "PE" THEN
                MOVE "PERNAMBUCO"          TO RELEST
            ELSE IF PREVEST EQUAL "PI" THEN
                MOVE "PIAUI"               TO RELEST
            ELSE IF PREVEST EQUAL "RJ" THEN
                MOVE "RIO DE JANEIRO"      TO RELEST
            ELSE IF PREVEST EQUAL "RN" THEN
                MOVE "RIO GRANDE DO NORTE" TO RELEST
            ELSE IF PREVEST EQUAL "RS" THEN
                MOVE "RIO GRANDE DO SUL"   TO RELEST
            ELSE IF PREVEST EQUAL "RO" THEN
                MOVE "RONDONIA"            TO RELEST
            ELSE IF PREVEST EQUAL "RR" THEN
                MOVE "RORAIMA"             TO RELEST
            ELSE IF PREVEST EQUAL "SC" THEN
                MOVE "SANTA CATARINA"      TO RELEST
            ELSE IF PREVEST EQUAL "SP" THEN
                MOVE "SAO PAULO"           TO RELEST
            ELSE IF PREVEST EQUAL "SE" THEN
                MOVE "SERGIPE"             TO RELEST
            ELSE IF PREVEST EQUAL "TO" THEN
                MOVE "TOCANTINS"           TO RELEST.

        TERMINO SECTION.
            CLOSE RELGER.