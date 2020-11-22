        IDENTIFICATION DIVISION.
        PROGRAM-ID. EM03EX03.
        AUTHOR. VINICIUS ALVES.
        INSTALLATION. FATEC-SP.
        DATE-WRITTEN. 01/11/2016.
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
            SELECT CADENT ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT TRAB   ASSIGN TO DISK.
            SELECT CADSAI ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            
        DATA DIVISION.
        FILE SECTION.
        
        FD ENTRADA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADENT.DAT".
        01 REGENT.
           02 CODENT PIC X (05).
           02 NOMEEN PIC X (30).
           02 DEPENT PIC 99.
           
        SD TRAB.
        01 REGTRAB.
           02 CODTRA PIC X (05).
           02 NOMETR PIC X (30).
           02 DEPTRA PIC 99.
           
        FD SAIDA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADSAI.DAT".
        01 REGSAI.
           02 CODSAI PIC X (05).
           02 NOMESA PIC X (30).
        
        WORKING-STORAGE SECTION.
        77 FIM-ARQ PIC XXX VALUE "NAO".
        
        PROCEDURE DIVISION.

        PGM.
            SORT TRAB
                 ASCENDING KEY CODTRA
                 USING ENTRADA
                 OUTPUT PROCEDURE ROTSAIDA        
        STOP RUN.
        
        ROTSAIDA SECTION.
            PERFORM INICIO.
            PERFORM PRINCIPAL
                    UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
            
        INICIO SECTION.
            OPEN OUTPUT SAIDA.
            PERFORM LESORT.
            
        LESORT SECTION.
            RETURN TRAB
                   AT END MOVE "SIM" TO FIM-ARQ.
                   
        PRINCIPAL SECTION.
            PERFORM GRAVACAO.
            PERFORM LESORT.
            
        GRAVACAO SECTION.
            MOVE CODTRA TO CODSAI.
            MOVE NOMETR TO NOMESA.
            WRITE REGSAI.
            
        TERMINO SECTION.
            CLOSE SAIDA.