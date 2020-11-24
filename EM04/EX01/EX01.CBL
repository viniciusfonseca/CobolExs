        IDENTIFICATION DIVISION.
        PROGRAM-ID. EM04EX01.
        AUTHOR. VINICIUS ALVES.
        INSTALLATION. FATEC-SP.
        DATE-WRITTEN. 24/11/2020.
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
            SELECT ARQA ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT ARQB ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT MERGE-ST ASSIGN TO DISK.
            SELECT ARQC ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.
            FD ARQA
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQA.DAT".
            01 REGA.
               02 COD-A   PIC 9(03).
               02 NOME-A  PIC X(30).
               02 TURMA-A PIC X(20).

            FD ARQB
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQB.DAT".
            01 REGB.
               02 COD-B   PIC 9(03).
               02 NOME-B  PIC X(30).
               02 TURMA-B PIC X(20).

            SD MERGE-ST.
            01 REGS.
               02 COD-S   PIC 9(03).
               02 NOME-S  PIC X(30).
               02 TURMA-S PIC X(20).

            FD ARQC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQC.DAT".
            01 REGC.
               02 COD-C   PIC 9(03).
               02 NOME-C  PIC X(30).
               02 TURMA-C PIC X(20).

        PROCEDURE DIVISION.
        PGM.
            MERGE MERGE-ST
               ON ASCENDING KEY COD-S
               USING ARQA ARQB
               GIVING ARQC.
            STOP RUN.