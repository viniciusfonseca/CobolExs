        IDENTIFICATION DIVISION.
        PROGRAM-ID. EM03EX01.
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
            01 REG-ENT.
               02 COD-ENT  PIC 9(05).
               02 NOM-ENT  PIC X(30).
               02 SEX-ENT  PIC X.
            
            SD TRAB.
            01 REG-TRAB.
               02 COD-TRAB PIC 9(05).
               02 NOM-TRAB PIC X(30).
               02 SEX-TRAB PIC X.
               
            FD CADSAI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADSAI.DAT".
            01 REG-SAI.
               02 COD-SAI  PIC 9(05).
               02 NOM-SAI  PIC X(30).
               02 SEX-SAI  PIC X.
               
        WORKING-STORAGE SECTION.
        77  FIM-ARQ        PIC X(03).
        
        PROCEDURE DIVISION.
        PGM.
        SORT TRAB
             ASCENDING KEY COD-TRAB
             ASCENDING KEY NOM-TRAB
             USING  CADENT
             GIVING CADSAI.
        STOP RUN.