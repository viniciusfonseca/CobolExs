        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM02EX06.
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
            SELECT CADOK  ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT RELCLI ASSIGN TO DISK.

        DATA DIVISION.

            FD CADCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCLI.DAT".
            01 REGCLI.
               02 CODCLI   PIC 9(05).
               02 CPF.
                  03 CPF-D PIC 9 OCCURS 9 TIMES. 
                  03 V1    PIC 9.
                  03 V2    PIC 9.
                02 NOMECLI PIC X(30).
                   88 NOMEINV VALUE SPACES.
                02 ESTCLI  PIC X(02).
                   88 ESTVALIDO VALUE 
                      "AC" "AL" "AP" "AM" "BA" "CE" 
                      "DF" "ES" "GO" "MA" "MT" "MS"
                      "MG" "PA" "PB" "PR" "PE" "PI" 
                      "RJ" "RN" "RS" "RO" "RR" "SC" 
                      "SP" "SE" "TO".
                02 CIDCLI  PIC X(30).
                   88 CIDINV VALUE SPACES.
                02 EMCLI   PIC X(30).
                   88 EMINV  VALUE SPACES.
                02 TELCLI  PIC 9(10).
                   
            FD CADOK
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADOK.DAT".
            01 REGOK.
               02 CODOK   PIC 9(05).
               02 CPFOK   PIC 9(09).
               02 NOMEOK  PIC X(30).
               02 ESTOK   PIC X(02).
               02 CIDOK   PIC X(30).
               02 EMOK    PIC X(30).
               02 TELOK   PIC 9(10).

            
            FD RELCLI
               LABEL RECORD IS OMITTED.    
            01 IMPCLI    PIC X(80) VALUE SPACES.

            WORKING-STORAGE SECTION.
            77 FIM-ARQ   PIC X(03) VALUE "NAO".
            77 CT-LIN    PIC 9(02) VALUE 40.
            77 CT-PAG    PIC X(02) VALUE ZEROES.

            77 AUXMULT   PIC 99 OCCURS 10 TIMES.
            77 AUXMULT2  PIC 99 OCCURS 10 TIMES.
            77 AUXV1     PIC 99.
            77 AUXV2     PIC 99.
            77 AUXTOM    PIC 99.

            77 VERDIG1   PIC 9.
            77 VERDIG2   PIC 9.

            77 AUXCPF    PIC 99999.

            77 REGVALIDO PIC X(03) VALUE "NAO".
               88 VALIDO VALUE "SIM".

            01 CAB-01.
               02 FILLER PIC X(32) VALUE              SPACES.
               02 FILLER PIC X(17) VALUE "TURISMAR TURISMOS".
               02 FILLER PIC X(31) VALUE              SPACES.

            01 CAB-02.
               02 FILLER PIC X(17) VALUE SPACES. 
               02 FILLER PIC X(22) VALUE 
                       "RELATORIO DE DADOS DE ".
               02 FILLER PIC X(23) VALUE
                      "CLIENTES INCONSISTENTES".
               02 FILLER PIC X(18) VALUE SPACES.

            01 CAB-03.
               02 FILLER PIC X(17) VALUE
                            "CODIGO DO CLIENTE".
               02 FILLER PIC X(04) VALUE SPACES.
               02 FILLER PIC X(15) VALUE
                              "DADOS INVALIDOS".
               02 FILLER PIC X(44) VALUE SPACES.

            01 CAB-04.
               02 FILLER PIC X(80) VALUE ALL "-".

            01 ERR-LN.
               02 IMPCOD    PIC X(05).
               02 FILLER    PIC X(12) VALUE SPACES.
               02 FILLER    PIC X(04) VALUE SPACES.
               02 MSGERR    PIC X(30).
               02 FILLER    PIC X(29) VALUE SPACES.


        PROCEDURE DIVISION.
        PGM.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO.
            MOVE 1  TO AUXV1.
            MOVE 11 TO AUXV2.
            PERFORM POPULA 10 TIMES.

            OPEN INPUT  CADCLI
                 OUTPUT CADOK
                        RELCLI.
            PERFORM LEITURA.

        POPULA.
            MOVE AUXV2 TO   AUXMULT(AUXV1).
            ADD      1 TO   AUXV1.
            SUBTRACT 1 FROM AUXV2.

        PRINCIPAL.
            PERFORM SAIDA.
            PERFORM LEITURA.

        SAIDA.
            MOVE "SIM"  TO REGVALIDO.

            PERFORM VALIDACPF.
            PERFORM VALIDANOM.
            PERFORM VALIDAEST.
            PERFORM VALIDACID.
            PERFORM VALIDAEM .

            IF VALIDO PERFORM GRAVACAO.

        IMPRESSAO.
            IF CT-LIN GREATER THAN 39
                PERFORM CABECALHO.
            PERFORM IMPERRO.

        CABECALHO.
            MOVE ZEROES TO CT-LIN.
            MOVE SPACES TO IMPCLI.

            WRITE IMPCLI             AFTER ADVANCING   PAGE.
            WRITE IMPCLI FROM CAB-01 AFTER ADVANCING 1 LINE.
            WRITE IMPCLI FROM CAB-02 AFTER ADVANCING 1 LINE.
            WRITE IMPCLI FROM CAB-03 AFTER ADVANCING 1 LINE.
            WRITE IMPCLI FROM CAB-04 AFTER ADVANCING 1 LINE.

        IMPERRO.
            MOVE CODCLI TO IMPCOD.
            WRITE IMPCLI FROM ERR-LN AFTER ADVANCING 1 LINE.
            MOVE  SPACES TO   IMPCOD.
            ADD 1 TO CT-LIN.

        GRAVACAO.
            MOVE CODCLI  TO CODOK.
            MOVE CPF     TO CPFOK.
            MOVE NOMECLI TO NOMEOK.
            MOVE ESTCLI  TO ESTOK.
            MOVE CIDCLI  TO CIDOK.
            MOVE EMCLI   TO EMOK.
            MOVE TELCLI  TO TELOK.
            WRITE REGOK.

        LEITURA.
            READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.

        VALIDACPF.
            MOVE 1 TO AUXV1.
            MOVE 2 TO AUXV2.
            PERFORM POPULABAIXO 9 TIMES.
            MOVE 1 TO AUXV1.
            MOVE ZEROES TO AUXCPF.
            PERFORM ACUMULA     9 TIMES.
            DIVIDE 11 INTO AUXCPF GIVING AUXV1 
                                  REMAINDER AUXCPF.

            IF AUXCPF LESS THAN 2
                MOVE 0 TO VERDIG1
            ELSE SUBTRACT AUXCPF FROM 11 GIVING VERDIG1.

            MOVE 1 TO AUXV1.
            MOVE 1 TO AUXV2.
            PERFORM POPULABAIXO 10 TIMES.
            MOVE 1 TO AUXV1.
            MOVE ZEROES TO AUXCPF.
            PERFORM ACUMULA     10 TIMES.
            DIVIDE 11 INTO AUXCPF GIVING AUXV2
                                  REMAINDER AUXCPF.

            IF AUXCPF LESS THAN 2
                MOVE 0 TO VERDIG2
            ELSE SUBTRACT AUXCPF FROM 11 GIVING VERDIG2.

            IF VERDIG1 NOT EQUAL V1 OR 
               VERDIG2 NOT EQUAL V2 THEN
                MOVE "NAO" TO REGVALIDO
                MOVE "CPF INVALIDO" TO MSGERR
                PERFORM IMPRESSAO
            END-IF.

        POPULABAIXO.
            IF AUXV1 IS EQUAL TO 10 THEN MOVE VERDIG1 TO AUXTOM
            ELSE MOVE CPF-D(AUXV1) TO AUXTOM.
            MULTIPLY AUXTOM BY AUXMULT(AUXV2) 
                            GIVING AUXMULT2(AUXV1).
            ADD 1 TO AUXV1.
            ADD 1 TO AUXV2.
        
        ACUMULA.
            ADD AUXMULT2(AUXV1) TO AUXCPF.
            ADD 1 TO AUXV1.

        VALIDANOM.
            IF NOMEINV THEN 
               MOVE "NAO" TO REGVALIDO
               MOVE "NOME NAO INFORMADO"    TO MSGERR
               PERFORM IMPRESSAO
            END-IF.
        
        VALIDAEST.
            IF NOT ESTVALIDO THEN
                MOVE "NAO" TO REGVALIDO
                MOVE "ESTADO INVALIDO"      TO MSGERR
                PERFORM IMPRESSAO
            END-IF.

        VALIDACID.
            IF CIDINV THEN
                MOVE "NAO" TO REGVALIDO
                MOVE "CIDADE NAO INFORMADA" TO MSGERR
                PERFORM IMPRESSAO
            END-IF.

        VALIDAEM.
            IF EMINV THEN
                MOVE "NAO" TO REGVALIDO
                MOVE "EMAIL SEM DESCRICAO"  TO MSGERR
                PERFORM IMPRESSAO
            END-IF.

        TERMINO.
            CLOSE CADCLI
                  CADOK
                  RELCLI.