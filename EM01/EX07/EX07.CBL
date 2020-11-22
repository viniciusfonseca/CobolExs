        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM01EX07.
        AUTHOR.        VINICIUS ALVES.
        INSTALLATION.  FATEC-SP.
        DATE-WRITTEN.  21/11/2020.
        DATE-COMPILED. 21/11/2020.
      * REMARKS. LÊ OS DADOS DOS ALUNOS EM CADALU.DAT E GRAVA EM
      * CADATU.DAT SOMENTE OS DADOS DOS ALUNOS DO SEXO FEMININO COM AS
      * RESPECTIVAS MÉDIAS CALCULADAS.

        ENVIRONMENT DIVISION.
            CONFIGURATION SECTION.
            SOURCE-COMPUTER. IBM-PC.
            OBJECT-COMPUTER. IBM-PC.
            SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
            SELECT CADALU ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT CADATU ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
            FILE SECTION.

            FD CADALU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADALU.DAT".
            01 REGENT.
               02 NUM-ENT  PIC 9(05).
               02 NOME-ENT PIC X(20).
               02 NT1-ENT  PIC 9(02)V99.
               02 NT2-ENT  PIC 9(02)V99.
               02 NT3-ENT  PIC 9(02)V99.
               02 NT4-ENT  PIC 9(02)V99.
               02 SEX-ENT  PIC X(01).

            FD CADATU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADATU.DAT".
            01 REGSAI.
               02 NUM-SAI   PIC 9(05).
               02 NOME-SAI  PIC X(20).
               02 MEDIA-SAI PIC 9(05)V99 VALUE ZEROS.
               02 SEX-SAI   PIC X(01).

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
                 OUTPUT CADATU.
            PERFORM LEITURA.

        LEITURA.
            READ CADALU AT END MOVE "SIM" TO FIM-ARQ.

        GRAVACAO.
            IF SEX-ENT IS NOT EQUAL TO "F" THEN EXIT SECTION.
            MOVE NUM-ENT TO NUM-SAI.
            MOVE NOME-ENT TO NOME-SAI.
            MOVE 0 TO MEDIA-SAI.
            ADD NT1-ENT TO MEDIA-SAI.
            ADD NT2-ENT TO MEDIA-SAI.
            ADD NT3-ENT TO MEDIA-SAI.
            ADD NT4-ENT TO MEDIA-SAI.
            DIVIDE 4 INTO MEDIA-SAI.
            WRITE REGSAI.

        PRINCIPAL.
            PERFORM GRAVACAO.
            PERFORM LEITURA.

        TERMINO.
            CLOSE CADALU
                  CADATU.
