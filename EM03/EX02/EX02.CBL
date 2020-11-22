        IDENTIFICATION DIVISION.
        PROGRAM-ID. EM03EX02.
        AUTHOR. VINICIUS ALVES.
        INSTALLATION. FATEC-SP.
        DATE-WRITTEN. 25/10/2016.
        DATE-COMPILED.
        SECURITY. ONLY AUTHOR MAY MODIFY.
      * REMARKS.
        
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
            SOURCE-COMPUTER. IBM-PC.
            OBJECT-COMPUTER. IBM-PC.
            SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
            
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT CADENT ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT TRAB ASSIGN TO DISK.
            SELECT CADSAI ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            
        DATA DIVISION.
        FILE SECTION.
            FD CADENT
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADENT.DAT".
            01 REGENT.
               02 COD-ENT PIC 9 (05).
               02 NOM-ENT PIC X (30).
               02 SEX-ENT PIC X.
               
            SD TRAB.
            01 REGTRAB.
               02 COD-TRAB PIC 9 (05).
               02 NOM-TRAB PIC X (30).
               02 SEX-TRAB PIC X.
               
            FD CADSAI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADSAI.DAT".
            01 REGTRAB.
               02 COD-TRAB PIC 9 (05).
               02 NOM-TRAB PIC X (30).
               
        WORKING-STORAGE SECTION.
        77  FIM-ARQ        PIC X (03) VALUE "NAO".
        
        PROCEDURE DIVISION.
        PGM.
            SORT TRAB
                 DESCENDING KEY  COD-ENT
                 INPUT PROCEDURE ROT-ENTRADA
                 GIVING          SAIDA.
        STOP RUN.
        
        ROT-ENTRADA SECTION.
            PERFORM INICIO-ENTRADA.
            PERFORM PRINCIPAL-ENTRADA 
                    UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM FIM-ENTRADA.
            
        INICIO-ENTRADA SECTION.
            OPEN INPUT ENTRADA.
            PERFORM LEITURA-ENTRADA.
            
        LEITURA-ENTRADA SECTION.
            READ CADENT AT END MOVE "SIM" TO FIM-ARQ.
        
        PRINCIPAL-ENTRADA SECTION.
            PERFORM SELECAO-ENTRADA.
            PERFORM LEITURA-ENTRADA.
            
        SELECAO-ENTRADA SECTION.
            IF SEX-ENT EQUAL "M" PERFORM GRAVACAO-ENTRADA.
        
        GRAVACAO-ENTRADA SECTION.
            MOVE COD-ENT TO COD-TRAB.
            MOVE NOM-ENT TO NOM-TRAB.
            MOVE SEX-ENT TO SEX-TRAB.
            RELEASE REGTRAB.
            
        FIM-ENTRADA SECTION.
            CLOSE CADENT.