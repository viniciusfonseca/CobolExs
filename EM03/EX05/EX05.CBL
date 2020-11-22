        IDENTIFICATION DIVISION.
        PROGRAM-ID. EM03EX05.
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
            SELECT ARQVENDA ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT TRAB   ASSIGN TO DISK.
            SELECT RELVENDA ASSIGN TO DISK.
            
        DATA DIVISION.
        FILE SECTION.
        
        FD ENTRADA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "ARQVENDA.DAT".
        01 REGENT.
           02 ESTAEN PIC XX.
           02 NRENT  PIC 9 (05).
           02 NOMEEN PIC X (20).
           02 VALENT PIC 9(07)V99.
           
        SD TRAB.
        01 REGTRA.
           02 ESTATR PIC XX.
           02 NRTRAB PIC 9 (05).
           02 NOMETR PIC X (20).
           02 VALTRA PIC 9(07)V99.
           
        FD RELFILE
           LABEL RECORD IS OMITTED.
        01 REGREL PIC X(80).
        
        WORKING-STORAGE SECTION.
        77 CT-LIN  PIC 99     VALUE 20.
        77 CT-PAG  PIC 99     VALUE ZEROES.
        77 FIM-ARQ PIC XXX    VALUE "NAO".

        77 PREVEST PIC XX     VALUE SPACES.
        77 MUDOULU PIC XXX    VALUE "NAO".

        77 PREVNR  PIC 9 (05).
        77 MUDOUNR PIC XXX    VALUE "NAO".
        
        01 CAB-01.
           02 FILLER PIC X (19) VALUE SPACES.
           02 FILLER PIC X (17) VALUE "VENDAS TOTAIS POR". 
           02 FILLER PIC X (18) VALUE " ESTADO - VENDEDOR".
           02 FILLER PIC X (18) VALUE SPACES.
           02 FILLER PIC X (05) VALUE "PAG. ".
           02 PAG-OU PIC ZZ9    VALUE ZEROES.

        01 CAB-02.
           02 FILLER PIC X (08) VALUE "ESTADO: ".
           02 RELEST PIC X (30) VALUE SPACES.
           02 FILLER PIC X (42) VALUE SPACES.

        01 CAB-03.
           02 FILLER PIC XX     VALUE SPACES.
           02 FILLER PIC X (15) VALUE "NRO DO VENDEDOR".
           02 FILLER PIC XX     VALUE SPACES.

           02 FILLER PIC XX     VALUE SPACES.
           02 FILLER PIC X (16) VALUE "NOME DO VENDEDOR".
           02 FILLER PIC X (04) VALUE SPACES.
           02 FILLER PIC XX     VALUE SPACES.

           02 FILLER PIC XX     VALUE SPACES.
           02 FILLER PIC X (14) VALUE "VALOR DA VENDA".
           02 FILLER PIC XX     VALUE SPACES.

        01 DETALHE.
           02 FILLER PIC XX     VALUE SPACES.
           02 NRRELA PIC X (05).
           02 FILLER PIC X (10) VALUE SPACES.
           02 FILLER PIC XX     VALUE SPACES.

           02 FILLER PIC XX     VALUE SPACES.
           02 NOMERE PIC X (20) VALUE SPACES.
           02 FILLER PIC XX     VALUE SPACES.

           02 FILLER PIC XX       VALUE SPACES.
           02 VALREL PIC 9(07)V99 VALUE SPACES.
           02 FILLER PIC XX       VALUE SPACES.

           02 FILLER PIC X (23)   VALUE SPACES.

        01 FIMVENDEDOR.
           02 FILLER PIC X (51)   VALUE SPACES.
           02 FILLER PIC X (19)   VALUE "TOTAL DO VENDEDOR: ".
           02 TOTVEN PIC 9(07)V99 VALUE ZEROES.
           
        01 FIMESTADO.
           02 FILLER PIC X (53)   VALUE SPACES.
           02 FILLER PIC X (17)   VALUE "TOTAL DO ESTADO: ".
           02 TOTEST PIC 9(07)V99 VALUE ZEROES.

        01 FIMRELATR.
           02 FILLER PIC X (50)   VALUE SPACES.
           02 FILLER PIC X (20)   VALUE "TOTAL DA COMPANHIA: ".
           02 TOTCOM PIC 9(07)V99 VALUE ZEROES.

        PROCEDURE DIVISION.

        PGM.
            SORT TRAB
                 ASCENDING KEY ESTAEN
                               CIDAEN
                               NOMEEN
                 USING ENTRADA
                 OUTPUT PROCEDURE ROTSAIDA        
        STOP RUN.

        ROTSAIDA SECTION.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.

        INICIO SECTION.
            OPEN OUTPUT RELFILE.
            PERFORM LEITURA.

        LEITURA SECTION.
            RETURN TRAB AT END MOVE "SIM" TO FIM-ARQ.

        PRINCIPAL SECTION.
            PERFORM IMPRESSAO.
            PERFORM LEITURA.

        VERIFICALU SECTION.
            MOVE "NAO" TO MUDOULU.
            IF PREVEST NOT EQUAL ESTATR THEN
                MOVE ESTATR TO PREVEST.
                MOVE "SIM"  TO MUDOULU.
            END-IF.

        CABECALHO SECTION.
            ADD   1       TO   CT-PAG.
            MOVE  CT-PAG  TO   PAG-OU.
            MOVE  SPACES  TO   REGREL.

            MOVE  ZEROES  TO   TOTEST.

            PERFORM ATRUBUIEST.

            WRITE REGREL              AFTER ADVANCING   PAGE .
            WRITE REGREL  FROM CAB-01 AFTER ADVANCING 1 LINE .
            WRITE REGREL  FROM CAB-02 AFTER ADVANCING 2 LINES.
            WRITE REGREL  FROM CAB-03 AFTER ADVANCING 2 LINES.

            MOVE  ZEROES  TO   CT-LIN.

        IMPRESSAO SECTION.
            PERFORM VERIFICALU.
            IF CT-LIN GREATER THAN 19 OR MUDOULU EQUAL "SIM"
            THEN
                IF CT-PAG GREATER THAN ZEROES THEN
                    PERFORM IMPEST
                PERFORM CABECALHO
            END-IF.
            PERFORM IMPDET.

        COPIAVAR SECTION.
            MOVE NRTRAB TO NRRELA.
            MOVE NOMETR TO NOMERE.
            MOVE VALTRA TO VALREL. 

        CONTABIL SECTION.
            ADD  VALTRA TO TOTVEN.
            ADD  VALTRA TO TOTEST.
            ADD  VALTRA TO TOTCOM.

        IMPDET SECTION.
            PERFORM COPIAVAR.
            PERFORM CONTABIL.
            PERFORM VERIFICANR.
            IF MUDOUNR NOT EQUAL "SIM" THEN
                MOVE SPACES TO NRRELA
                MOVE SPACES TO NOMERE
            END-IF.
            WRITE REGREL FROM DETALHE AFTER ADVANCING 1 LINE.
            IF MUDOUNR EQUAL "SIM" THEN
                PERFORM IMPVENDEDOR.
            ADD 1 TO CT-LIN.

        VERIFICANR SECTION.
            MOVE "NAO" TO MUDOUNR.
            IF PREVNR NOT EQUAL NRTRAB THEN
                MOVE "SIM" TO MUDOUNR.

        IMPVENDEDOR SECTION.
            WRITE REGREL  FROM  FIMVENDEDOR
                          AFTER ADVANCING 2 LINES.
            MOVE ZEROES TO TOTVEN.

        IMPEST SECTION.
            WRITE REGREL  FROM  FIMESTADO 
                          AFTER ADVANCING 2 LINES.
            MOVE ZEROES TO TOTEST.

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

        IMPROD SECTION.
            WRITE REGREL  FROM  FIMRELATR 
                          AFTER ADVANCING 2 LINES.

        TERMINO SECTION.
            PERFORM IMPROD.
            CLOSE OUTPUT.