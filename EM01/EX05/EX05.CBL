        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM01EX05.
        AUTHOR.        VINICIUS ALVES.
        INSTALLATION.  FATEC-SP.
        DATE-WRITTEN.  21/11/2020.
        DATE-COMPILED. 21/11/2020.
      * REMARKS. LÊ OS DADOS DOS ALUNOS EM CADALU.DAT E GRAVA EM
      * CADAPR.DAT SOMENTE OS ALUNOS APROVADOS (FALTAS MENOR QUE 18 E
      * MÉDIA MAIOR OU IGUAL A 7).

        ENVIRONMENT DIVISION.
            CONFIGURATION SECTION.
            SOURCE-COMPUTER. IBM-PC.
            OBJECT-COMPUTER. IBM-PC.
            SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
            SELECT CADALU ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT CADAPR ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
            FILE SECTION.

            FD CADALU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADALU.DAT".
            01 REGALU.
               02 NUM-ENT  PIC 9(05).
               02 NOME-ENT PIC X(20).
               02 NT1-ENT  PIC 9(02)V99.
               02 NT2-ENT  PIC 9(02)V99.
               02 FLT-ENT  PIC 9(02).

            FD CADAPR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADAPR.DAT".
            01 REGAPR.
               02 NUM-SAI   PIC 9(05).
               02 NOME-SAI  PIC X(20).
               02 MEDIA-SAI PIC 9(02)V99 VALUE ZEROS.

            WORKING-STORAGE SECTION.
            77 FIM-ARQ PIC X(03) VALUE "NAO".
        
        PROCEDURE DIVISION.
        PGM.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO.
            OPEN INPUT  CADALU
                 OUTPUT CADAPR.
            PERFORM LEITURA.

        LEITURA.
            READ CADALU AT END MOVE "SIM" TO FIM-ARQ.

        GRAVACAO.
            IF FLT-ENT IS GREATER THAN 18 THEN EXIT SECTION.
            MOVE NUM-ENT  TO NUM-SAI.
            MOVE NOME-ENT TO NOME-SAI.
            MOVE 0 TO MEDIA-SAI.
            ADD NT1-ENT TO MEDIA-SAI.
            ADD NT2-ENT TO MEDIA-SAI.
            DIVIDE 2 INTO MEDIA-SAI.
            IF MEDIA-SAI IS NOT GREATER THAN 7 THEN EXIT SECTION.
            WRITE REGAPR.

        PRINCIPAL.
            PERFORM GRAVACAO.
            PERFORM LEITURA.

        TERMINO.
            CLOSE CADALU
                  CADAPR.
